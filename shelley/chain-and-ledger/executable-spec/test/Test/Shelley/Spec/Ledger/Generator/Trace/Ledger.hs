{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-- The HasTrace instance relies on test generators and so cannot
-- be included with the LEDGER STS
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Test.Shelley.Spec.Ledger.Generator.Trace.Ledger where

import           Control.Monad (foldM)
import           Data.Functor.Identity (runIdentity)
import qualified Data.Sequence as Seq
import           Test.QuickCheck (Gen)

import           Control.Monad.Trans.Reader (runReaderT)
import           Control.State.Transition.Extended (IRC, TRC (..), applySTS)
import qualified Control.State.Transition.Trace.Generator.QuickCheck as TQC
import           Shelley.Spec.Ledger.BaseTypes (Globals)

import           Shelley.Spec.Ledger.LedgerState (pattern LedgerState, genesisState)
import           Shelley.Spec.Ledger.Slot (SlotNo (..))
import           Shelley.Spec.Ledger.STS.Ledger (LedgerEnv (..))
import           Shelley.Spec.Ledger.STS.Ledgers (LedgersEnv (..))
import           Shelley.Spec.Ledger.TxData (Ix)

import           Test.Shelley.Spec.Ledger.ConcreteCryptoTypes (DPState, LEDGER, LEDGERS, Tx,
                     UTxOState)
import           Test.Shelley.Spec.Ledger.Generator.Constants (maxGenesisUTxOouts, maxTxsPerBlock,
                     minGenesisUTxOouts)
import           Test.Shelley.Spec.Ledger.Generator.Core (KeySpace(..), genCoin)
import           Test.Shelley.Spec.Ledger.Generator.Update (genPParams)
import           Test.Shelley.Spec.Ledger.Generator.Utxo (genTx)
import           Test.Shelley.Spec.Ledger.Shrinkers (shrinkTx)
import           Test.Shelley.Spec.Ledger.Generator.Presets (genUtxo0, genesisDelegs0)
import           Test.Shelley.Spec.Ledger.Utils (runShelleyBase)

-- The LEDGER STS combines utxo and delegation rules and allows for generating transactions
-- with meaningful delegation certificates.
instance TQC.HasTrace LEDGER KeySpace where
  envGen _ =
    LedgerEnv <$> pure (SlotNo 0)
              <*> pure 0
              <*> genPParams
              <*> genCoin 1000000 10000000

  sigGen = genTx

  shrinkSignal = shrinkTx

  type BaseEnv LEDGER = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals

instance TQC.HasTrace LEDGERS KeySpace where
  envGen _ =
    LedgersEnv <$> pure (SlotNo 0)
               <*> genPParams
               <*> genCoin 1000000 10000000

  -- a LEDGERS signal is a sequence of LEDGER signals
  sigGen ks (LedgersEnv slotNo pParams reserves) (LedgerState utxoSt dpSt) = do
      (_, _, txs') <-
        foldM genAndApplyTx
              (utxoSt, dpSt, [])
              [0 .. (fromIntegral maxTxsPerBlock - 1)]

      pure $ Seq.fromList (reverse txs') -- reverse Newest first to Oldest first
    where
      genAndApplyTx
        :: (UTxOState, DPState, [Tx])
        -> Ix
        -> Gen (UTxOState, DPState, [Tx])
      genAndApplyTx (u, dp, txs) ix = do
        let ledgerEnv = LedgerEnv slotNo ix pParams reserves
        tx <- genTx ks ledgerEnv (u, dp)

        let res = runShelleyBase $ applySTS @LEDGER (TRC (ledgerEnv, (u, dp), tx))
        pure $ case res of
          Left _ ->
            (u, dp, txs)
          Right (u',dp') ->
            (u',dp', tx:txs)

  shrinkSignal = const []

  type BaseEnv LEDGERS = Globals
  interpretSTS globals act = runIdentity $ runReaderT act globals

-- | Generate initial state for the LEDGER STS using the STS environment.
--
-- Note: this function must be usable in place of 'applySTS' and needs to align
-- with the signature 'RuleContext sts -> Gen (Either [[PredicateFailure sts]] (State sts))'.
-- To achieve this we (1) use 'IRC LEDGER' (the "initial rule context") instead of simply 'LedgerEnv'
-- and (2) always return Right (since this function does not raise predicate failures).
mkGenesisLedgerState
  :: IRC LEDGER
  -> Gen (Either a (UTxOState, DPState))
mkGenesisLedgerState _ = do
  utxo0 <- genUtxo0 minGenesisUTxOouts maxGenesisUTxOouts
  let (LedgerState utxoSt dpSt) = genesisState genesisDelegs0 utxo0
  pure $ Right (utxoSt, dpSt)
