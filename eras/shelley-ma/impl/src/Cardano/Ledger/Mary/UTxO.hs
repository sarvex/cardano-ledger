{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Cardano.Ledger.Mary.UTxO (getConsumedMaryValue) where

import Cardano.Ledger.Coin (Coin)
import Cardano.Ledger.Core
import Cardano.Ledger.Crypto
import Cardano.Ledger.Mary.Value (MaryValue)
import Cardano.Ledger.Shelley.PParams (ShelleyPParamsHKD (..))
import Cardano.Ledger.Shelley.TxBody (ShelleyEraTxBody (..), Wdrl (..))
import Cardano.Ledger.Shelley.UTxO
  ( EraUTxO (..),
    ShelleyScriptsNeeded (..),
    UTxO (UTxO),
    balance,
    getShelleyScriptsNeeded,
    keyRefunds,
  )
import Cardano.Ledger.ShelleyMA.Era (MaryOrAllegra (Mary), ShelleyMAEra)
import Cardano.Ledger.ShelleyMA.TxBody (ShelleyMAEraTxBody (..))
import Cardano.Ledger.Val (inject)
import Data.Foldable (fold)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import GHC.Records (HasField)
import Lens.Micro

instance Crypto c => EraUTxO (ShelleyMAEra 'Mary c) where
  type ScriptsNeeded (ShelleyMAEra 'Mary c) = ShelleyScriptsNeeded (ShelleyMAEra 'Mary c)

  getConsumedValue = getConsumedMaryValue

  getScriptsNeeded = getMaryScriptsNeeded

  getScriptsHashesNeeded (ShelleyScriptsNeeded scriptHashes) = scriptHashes

-- | Calculate the value consumed by the transation.
--
--   This differs from the corresponding Shelley function 'Shelley.coinConsumed'
--   since it works on Value and it also considers the "mint" field which
--   creates or destroys non-Ada tokens.
--
--   Note that this is slightly confusing, since it also covers non-Ada assets
--   _created_ by the transaction, depending on the sign of the quantities in
--   the mint field.
getConsumedMaryValue ::
  ( ShelleyMAEraTxBody era,
    Value era ~ MaryValue (EraCrypto era),
    HasField "_keyDeposit" (PParams era) Coin
  ) =>
  PParams era ->
  UTxO era ->
  TxBody era ->
  MaryValue (EraCrypto era)
getConsumedMaryValue pp (UTxO u) txBody = consumedValue <> txBody ^. mintValueTxBodyF
  where
    {- balance (txins tx ◁ u) + wbalance (txwdrls tx) + keyRefunds pp tx -}
    consumedValue =
      balance (UTxO (Map.restrictKeys u (txBody ^. inputsTxBodyL)))
        <> inject (refunds <> withdrawals)
    refunds = keyRefunds pp txBody
    withdrawals = fold . unWdrl $ txBody ^. wdrlsTxBodyL

-- | Computes the set of script hashes required to unlock the transaction inputs and the
-- withdrawals. Unlike the one from Shelley, this one also includes script hashes needed
-- for minting multi-assets in the transaction.
getMaryScriptsNeeded ::
  ShelleyMAEraTxBody era =>
  UTxO era ->
  TxBody era ->
  ShelleyScriptsNeeded era
getMaryScriptsNeeded u txBody =
  case getShelleyScriptsNeeded u txBody of
    ShelleyScriptsNeeded shelleyScriptsNeeded ->
      ShelleyScriptsNeeded (shelleyScriptsNeeded `Set.union` (txBody ^. mintedTxBodyF))