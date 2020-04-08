{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Shelley.Spec.Ledger.Generator.Update
  ( genPParams
  , genUpdate
  )
  where

import           Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import           Data.Maybe (fromMaybe)
import           Data.Ratio ((%))

import           Test.QuickCheck (Gen)
import qualified Test.QuickCheck as QC

import           Numeric.Natural (Natural)
import           Shelley.Spec.Ledger.BaseTypes (Nonce (NeutralNonce), UnitInterval, mkNonce)
import           Shelley.Spec.Ledger.Coin (Coin (..))
import           Shelley.Spec.Ledger.Keys (GenDelegs (..), hashKey, vKey)
import           Shelley.Spec.Ledger.LedgerState (_dstate, _genDelegs)
import           Shelley.Spec.Ledger.PParams (ActiveSlotCoeff, PParams, PParams' (PParams),
                     pattern ProposedPPUpdates, ProtVer (..), pattern Update, mkActiveSlotCoeff,
                     _a0, _activeSlotCoeff, _d, _eMax, _extraEntropy, _keyDecayRate, _keyDeposit,
                     _keyMinRefund, _maxBBSize, _maxBHSize, _maxTxSize, _minfeeA, _minfeeB, _nOpt,
                     _poolDecayRate, _poolDeposit, _poolMinRefund, _protocolVersion,
                     _protocolVersion, _rho, _tau)
import           Shelley.Spec.Ledger.Slot (EpochNo (EpochNo), SlotNo)

import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (CoreKeyPair, DPState, GenKeyHash,
                     KeyHash, KeyPair, ProposedPPUpdates, UTxOState, Update)
import           Test.Shelley.Spec.Ledger.Examples.Examples (unsafeMkUnitInterval)
import           Test.Shelley.Spec.Ledger.Generator.Constants (frequencyLowMaxEpoch,
                     frequencyTxUpdates, maxMinFeeA, maxMinFeeB)
import           Test.Shelley.Spec.Ledger.Generator.Core (AllPoolKeys (cold), genInteger,
                     genNatural, genWord64, increasingProbabilityAt, tooLateInEpoch)
import           Test.Shelley.Spec.Ledger.Utils (epochFromSlotNo)

genRationalInThousands :: Integer -> Integer -> Gen Rational
genRationalInThousands lower upper =
  (% 1000) <$> genInteger lower upper

genIntervalInThousands :: Integer -> Integer -> Gen UnitInterval
genIntervalInThousands lower upper =
  unsafeMkUnitInterval <$> genRationalInThousands lower upper

genPParams :: Gen PParams
genPParams = mkPParams <$> genNatural 0 maxMinFeeA -- _minfeeA
                       <*> genNatural 0 maxMinFeeB -- _minfeeB
                       <*> szGen  -- (maxBBSize, maxBHSize, maxTxSize)
                       <*> genKeyDeposit
                       <*> genKeyMinRefund
                       <*> genKeyDecayRate
                       <*> genPoolDeposit
                       <*> genPoolMinRefund
                       <*> genPoolDecayRate
                       <*> genEMax
                       <*> genNOpt
                       <*> genA0
                       <*> genRho
                       <*> genTau
                       <*> genActiveSlotCoeff
                       <*> genDecentralisationParam
                       <*> genExtraEntropy
                       <*> genProtocolVersion
  where

    -- | Generates max block, header and transaction size. First generates the
    -- body size and then header and tx sizes no larger than half the body size.
    szGen :: Gen (Natural, Natural, Natural)
    szGen = do
      blockBodySize <- genNatural low hi
      (blockBodySize,,)
        <$> rangeUpTo (blockBodySize `div` 2)
        <*> rangeUpTo (blockBodySize `div` 2)

    -- A wrapper to enable the dependent generators for the max sizes
    mkPParams minFeeA minFeeB (maxBBSize, maxTxSize, maxBHSize) =
      PParams minFeeA minFeeB maxBBSize maxTxSize maxBHSize

    rangeUpTo :: Natural -> Gen Natural
    rangeUpTo upper = genNatural low upper

-- keyDecayRate: 0.001-0.1
genKeyDecayRate :: Gen Rational
genKeyDecayRate = genRationalInThousands 1 100


-- poolDeposit
-- NOTE: we need to keep these deposits small, otherwise
-- when we generate sequences of transactions we will bleed too
-- much funds into the deposit pool (i.e. funds not available as utxo)
genPoolDeposit :: Gen Coin
genPoolDeposit =
    increasingProbabilityAt
          (Coin <$> genInteger 0 100)
          (Coin 0, Coin 100)

-- poolMinRefund: 0.1-0.7
genPoolMinRefund :: Gen UnitInterval
genPoolMinRefund = genIntervalInThousands 100 700

-- poolDecayRate: 0.001-0.1
genPoolDecayRate :: Gen Rational
genPoolDecayRate = genRationalInThousands 1 100

-- Generates a Neutral or actual Nonces with equal frequency
genExtraEntropy :: Gen Nonce
genExtraEntropy  = QC.frequency [ (1, pure NeutralNonce)
                                , (1, mkNonce <$> genNatural 1 123)]


-- Note: we keep the lower bound high enough so that we can more likely
-- generate valid transactions and blocks
low, hi :: Natural
low = 50000
hi = 200000

-- keyMinRefund: 0.1-0.5
genKeyMinRefund :: Gen UnitInterval
genKeyMinRefund = genIntervalInThousands 100 500

-- eMax (for an epoch per 5 days, say, this is between a month and 7yrs)
genEMax :: Gen EpochNo
genEMax = EpochNo <$> genWord64 frequencyLowMaxEpoch 500

-- | nOpt
genNOpt :: Gen Natural
genNOpt  = genNatural 1 100

-- | genKeyDeposit
-- NOTE: we need to keep these deposits small, otherwise
-- when we generate sequences of transactions we will bleed too
-- much funds into the deposit pool (i.e. funds not available as utxo)
genKeyDeposit :: Gen Coin
genKeyDeposit = increasingProbabilityAt
                  (Coin <$> genInteger 0 20)
                  (Coin 0, Coin 20)

-- | a0: 0.01-1.0
genA0 :: Gen Rational
genA0 = genRationalInThousands 10 1000

-- | rho: 0.001-0.009
genRho :: Gen UnitInterval
genRho = genIntervalInThousands 1 9

-- | tau: 0.1-0.3
genTau :: Gen UnitInterval
genTau = genIntervalInThousands 100 300

-- | activeSlotCoeff: 0.1-1
genActiveSlotCoeff :: Gen ActiveSlotCoeff
genActiveSlotCoeff =
  (mkActiveSlotCoeff . unsafeMkUnitInterval)
  <$> QC.elements [0.025, 0.05, 0.075, 0.1, 0.2, 0.5]
-- ^^ This is a somewhat arbitrary group of values.
-- In the real system, we will probably be using a value near 1/10,
-- and we know that we would not ever choose values too small (say below 1/40)
-- or greater than a 1/2.

genDecentralisationParam :: Gen UnitInterval
genDecentralisationParam = unsafeMkUnitInterval <$> QC.elements [0.1, 0.2 .. 1]
-- ^^ TODO jc - generating d=0 takes some care, if there are no registered
-- stake pools then d=0 deadlocks the system.

genProtocolVersion :: Gen ProtVer
genProtocolVersion  = ProtVer <$> genNatural 1 10 <*> genNatural 1 50

-- | Generate a possible next Protocol version based on the previous version.
-- Increments the Major or Minor versions and possibly the Alt version.
genNextProtocolVersion
  :: PParams
  -> Gen ProtVer
genNextProtocolVersion pp = do
  QC.elements
    [ ProtVer (m + 1) 0
    , ProtVer m       (n + 1)]
  where
    ProtVer m n = _protocolVersion pp

-- | Generate a proposal for protocol parameter updates for all the given genesis keys.
-- Return an empty update if it is too late in the epoch for updates.
genPPUpdate
  :: SlotNo
  -> PParams
  -> [GenKeyHash]
  -> Gen ProposedPPUpdates
genPPUpdate s pp genesisKeys =
  if (tooLateInEpoch s)
    then
      pure (ProposedPPUpdates Map.empty)
    else do -- TODO generate Maybe tyes so not all updates are full
      minFeeA               <- genNatural 0 maxMinFeeA
      minFeeB               <- genNatural 0 maxMinFeeB
      maxBBSize             <- genNatural low hi
      maxTxSize             <- genNatural low hi
      maxBHSize             <- genNatural low hi
      keyDeposit            <- genKeyDeposit
      keyMinRefund          <- genKeyMinRefund
      keyDecayRate          <- genKeyDecayRate
      poolDeposit           <- genPoolDeposit
      poolMinRefund         <- genPoolMinRefund
      poolDecayRate         <- genPoolDecayRate
      eMax                  <- genEMax
      nopt                  <- genNOpt
      a0                    <- genA0
      rho                   <- genRho
      tau                   <- genTau
      activeSlotCoefficient <- genActiveSlotCoeff
      d                     <- genDecentralisationParam
      extraEntropy          <- genExtraEntropy
      protocolVersion       <- genNextProtocolVersion pp
      let pps = PParams { _minfeeA               = Just minFeeA
                        , _minfeeB               = Just minFeeB
                        , _maxBBSize             = Just maxBBSize
                        , _maxTxSize             = Just maxTxSize
                        , _maxBHSize             = Just maxBHSize
                        , _keyDeposit            = Just keyDeposit
                        , _keyMinRefund          = Just keyMinRefund
                        , _keyDecayRate          = Just keyDecayRate
                        , _poolDeposit           = Just poolDeposit
                        , _poolMinRefund         = Just poolMinRefund
                        , _poolDecayRate         = Just poolDecayRate
                        , _eMax                  = Just eMax
                        , _nOpt                  = Just nopt
                        , _a0                    = Just a0
                        , _rho                   = Just rho
                        , _tau                   = Just tau
                        , _activeSlotCoeff       = Just activeSlotCoefficient
                        , _d                     = Just d
                        , _extraEntropy          = Just extraEntropy
                        , _protocolVersion       = Just protocolVersion
                        }
      let ppUpdate = zip genesisKeys (repeat pps)
      pure $
        (ProposedPPUpdates . Map.fromList) ppUpdate

-- | Generate an @Update (where all the given nodes participate)
-- with a 50% chance of having non-empty PPUpdates or AVUpdates
-- and a 25% chance of both being empty or non-empty
genUpdateForNodes
  :: SlotNo
  -> EpochNo -- current epoch
  -> [CoreKeyPair]
  -> PParams
  -> Gen (Maybe Update)
genUpdateForNodes s e coreKeys pp =
  Just <$> (Update <$> genPPUpdate_ <*> pure e)
  where
    genesisKeys = hashKey . vKey <$> coreKeys
    genPPUpdate_ = genPPUpdate s pp genesisKeys

-- | Occasionally generate an update and return with the witness keys
genUpdate
  :: SlotNo
  -> [(CoreKeyPair, AllPoolKeys)]
  -> Map KeyHash KeyPair -- indexed keys By StakeHash
  -> PParams
  -> (UTxOState, DPState)
  -> Gen (Maybe Update, [KeyPair])
genUpdate s coreKeyPairs keysByStakeHash pp (_utxoSt, delegPoolSt) = do
  nodes <- take 5 <$> QC.shuffle coreKeyPairs

  let e = epochFromSlotNo s
      (GenDelegs genDelegs) = (_genDelegs . _dstate) delegPoolSt
      genesisKeys = (fst . unzip) nodes
      updateWitnesses = latestPoolColdKey genDelegs <$> nodes

  QC.frequency [ ( frequencyTxUpdates
                 , (,updateWitnesses) <$> genUpdateForNodes s e genesisKeys pp)
               , ( 100 - frequencyTxUpdates
                 , pure (Nothing, []))]

  where
    -- | Lookup the cold key for the given node in the genesis delegations map.
    -- Then lookup the cold key hash in the `keysByStakeHash` reverse index.
    -- If we find the key there, we can assume that a GenesisDeleg certificate
    -- has changed the cold key, in which case we use the new key (otherwise we
    -- can use the original cold key)
    latestPoolColdKey genDelegs_ (gkey, pkeys) =
      case Map.lookup (hashKey . vKey $ gkey) genDelegs_ of
          Nothing ->
            error "genUpdate: NoGenesisStaking"
          Just gkeyHash ->
            fromMaybe (cold pkeys)
                      (Map.lookup gkeyHash keysByStakeHash)