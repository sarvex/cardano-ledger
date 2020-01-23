{-# Language TypeApplications #-}
{-# Language OverloadedStrings #-}
{-# Language RankNTypes #-}


module Test.Cardano.Chain.Byron.Auxiliary
  ( genApplyMempoolPayloadErr
  , ts_roundTripApplyMempoolPayloadErrCompat
  , ts_scheduledDelegations
  , tests
  )
  where

import Cardano.Prelude
  -- import Test.Cardano.Prelude

import Cardano.Crypto (ProtocolMagicId)
import Cardano.Chain.Byron.Auxiliary
  ( ApplyMempoolPayloadErr (..)
  , applyScheduledDelegations
  , getDelegationMap
  , applyChainTick
  , getScheduledDelegations
  )
import Cardano.Spec.Chain.STS.Rule.Chain (CHAIN)
import qualified Control.State.Transition.Trace as STS
import qualified Control.State.Transition.Generator as STS

import Cardano.Chain.Block.Validation (ChainValidationState (..), initialChainValidationState)
import Cardano.Chain.Slotting (SlotNumber(..), SlotCount (..))
import Cardano.Chain.Delegation.Validation.Scheduling (ScheduledDelegation(..))

import Test.Cardano.Chain.Elaboration.Block (abEnvToCfg, transactionIds)
import Test.Cardano.Chain.UTxO.Gen (genUTxOValidationError)
import qualified Test.Cardano.Chain.Delegation.Gen as Dlg
import qualified Test.Cardano.Chain.Update.Gen as UpdateIface
import Test.Cardano.Crypto.Gen (feedPM)
import Test.Options (eachOfTS, TSProperty)
import Test.Cardano.Binary.Helpers.GoldenRoundTrip (roundTripsCBORShow)

import qualified Data.Sequence as Seq

import Hedgehog (Gen, property, forAll, Group(..), (===))
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

import Test.Options (withTestsTS, TSGroup)

import Test.Cardano.Chain.UTxO.Model (elaborateInitialUTxO)
import Test.Cardano.Chain.Block.Model (elaborateAndUpdate)

import Cardano.Chain.Genesis.Config (configSlotSecurityParam)
import qualified Cardano.Chain.Genesis.Config as Genesis

tests :: TSGroup
tests scenario = Group "Test.Cardano.Chain.Byron.Auxiliary" 
  [ ( "ts_scheduledDelegations", ts_scheduledDelegations scenario)
  , ( "ts_chainTick", ts_chainTick scenario) ]

ts_roundTripApplyMempoolPayloadErrCompat :: TSProperty
ts_roundTripApplyMempoolPayloadErrCompat = eachOfTS
  20
  (feedPM genApplyMempoolPayloadErr)
  roundTripsCBORShow


genApplyMempoolPayloadErr :: ProtocolMagicId -> Gen ApplyMempoolPayloadErr
genApplyMempoolPayloadErr pm = Gen.choice
  [ MempoolTxErr <$> genUTxOValidationError
  , MempoolDlgErr <$> Dlg.genError
  , MempoolUpdateProposalErr <$> UpdateIface.genError pm
  , MempoolUpdateVoteErr <$> UpdateIface.genError pm
  ]
setupChainValidationState :: STS.Trace CHAIN -> (ChainValidationState, Genesis.Config)
setupChainValidationState sampleTrace =
  let chainEnv@(_, abstractInitialUTxO,_,_,_) = STS._traceEnv sampleTrace
      config = abEnvToCfg chainEnv
      (initialUTxO, txIdMap) = elaborateInitialUTxO abstractInitialUTxO
      initialAbstractToConcreteIdMaps = mempty { transactionIds = txIdMap }
      initialStateNoUTxO = either (panic . show) identity $ initialChainValidationState config
      initialState = initialStateNoUTxO { cvsUtxo = initialUTxO }
      (cvs, _) = either (panic . show) identity $
        foldM
          (elaborateAndUpdate config)
          (initialState, initialAbstractToConcreteIdMaps)
          (STS.preStatesAndSignals STS.OldestFirst sampleTrace)
   in (cvs, config)

-- | getDelegationMap . applyChainTick slot == 
-- | (applyScheduledDelegations . getScheduledDelegations slot) 
ts_scheduledDelegations :: TSProperty
ts_scheduledDelegations = withTestsTS 100 . property $ do
  let traceLength = 10 :: Word64
  sampleTrace <- forAll $ STS.trace @CHAIN () traceLength
  let (cvs, config) = setupChainValidationState sampleTrace
      n = unSlotNumber . cvsLastSlot $ cvs
      k = unSlotCount . configSlotSecurityParam $ config
  slotNumber <- forAll $ SlotNumber <$> Gen.word64 (Range.linear n (n + 2*k))
  let tickedDelegationMap = getDelegationMap $ applyChainTick config slotNumber cvs
      sched = Seq.filter (\del -> sdSlot del <= slotNumber) $ getScheduledDelegations cvs
      anachronisticDelegationMap = applyScheduledDelegations sched (getDelegationMap cvs)
  tickedDelegationMap === anachronisticDelegationMap

ts_chainTick :: TSProperty
ts_chainTick = withTestsTS 100 . property $ do
  let traceLength = 10 :: Word64
  sampleTrace <- forAll $ STS.trace @CHAIN () traceLength
  let (cvs, config) = setupChainValidationState sampleTrace
      n0 = unSlotNumber . cvsLastSlot $ cvs
      k = unSlotCount . configSlotSecurityParam $ config
  n2 <- forAll $ Gen.word64 (Range.linear n0 (n0 + 2*k))
  n1 <- forAll $ Gen.word64 (Range.linear n0 n2)
  let tick n = applyChainTick config (SlotNumber n)
  (tick n2 . tick n1) cvs === tick n2 cvs

