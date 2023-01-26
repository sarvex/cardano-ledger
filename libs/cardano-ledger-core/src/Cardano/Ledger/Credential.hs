{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

module Cardano.Ledger.Credential (
  Credential (KeyHashObj, ScriptHashObj),
  GenesisCredential (..),
  PaymentCredential,
  Ptr (Ptr),
  ptrSlotNo,
  ptrTxIx,
  ptrCertIx,
  StakeCredential,
  StakeReference (..),
)
where

import Cardano.Ledger.BaseTypes (
  CertIx (..),
  TxIx (..),
  invalidKey,
 )
import Cardano.Ledger.Binary (
  DecCBOR (..),
  EncCBOR (..),
  FromCBOR (..),
  ToCBOR (..),
 )
import qualified Cardano.Ledger.Binary.Plain as Plain
import Cardano.Ledger.Crypto (Crypto)
import Cardano.Ledger.Hashes (ScriptHash)
import Cardano.Ledger.Keys (
  HasKeyRole (..),
  KeyHash,
  KeyRole (..),
 )
import Cardano.Ledger.Slot (SlotNo (..))
import Cardano.Ledger.TreeDiff (ToExpr)
import Control.DeepSeq (NFData)
import Data.Aeson (
  FromJSON (..),
  FromJSONKey,
  ToJSON (..),
  ToJSONKey,
  (.:),
  (.=),
 )
import qualified Data.Aeson as Aeson
import Data.Foldable (asum)
import Data.Typeable (Typeable)
import Data.Word
import GHC.Generics (Generic)
import NoThunks.Class (NoThunks (..))
import Quiet (Quiet (Quiet))

-- | Script hash or key hash for a payment or a staking object.
--
-- Note that credentials (unlike raw key hashes) do appear to vary from era to
-- era, since they reference the hash of a script, which can change. This
-- parameter is a phantom, however, so in actuality the instances will remain
-- the same.
data Credential (kr :: KeyRole) c
  = ScriptHashObj !(ScriptHash c)
  | KeyHashObj !(KeyHash kr c)
  deriving (Show, Eq, Generic, NFData, Ord)

instance HasKeyRole Credential where
  coerceKeyRole (ScriptHashObj x) = ScriptHashObj x
  coerceKeyRole (KeyHashObj x) = KeyHashObj $ coerceKeyRole x

instance NoThunks (Credential kr c)

instance Crypto c => ToJSON (Credential kr c) where
  toJSON (ScriptHashObj hash) =
    Aeson.object
      [ "script hash" .= hash
      ]
  toJSON (KeyHashObj hash) =
    Aeson.object
      [ "key hash" .= hash
      ]

instance Crypto c => FromJSON (Credential kr c) where
  parseJSON =
    Aeson.withObject "Credential" $ \obj ->
      asum [parser1 obj, parser2 obj]
    where
      parser1 obj = ScriptHashObj <$> obj .: "script hash"
      parser2 obj = KeyHashObj <$> obj .: "key hash"

instance Crypto c => ToJSONKey (Credential kr c)

instance Crypto c => FromJSONKey (Credential kr c)

type PaymentCredential c = Credential 'Payment c

type StakeCredential c = Credential 'Staking c

data StakeReference c
  = StakeRefBase !(StakeCredential c)
  | StakeRefPtr !Ptr
  | StakeRefNull
  deriving (Show, Eq, Generic, NFData, Ord)

instance NoThunks (StakeReference c)

-- TODO: implement this optimization:
-- We expect that `SlotNo` will fit into `Word32` for a very long time,
-- because we can assume that the rate at which it is incremented isn't going to
-- increase in the near future. Therefore with current rate we should be fine for
-- another 134 years. I suggest to remove this optimization in about a
-- hundred years or thereabouts, so around a year 2122 would be good.
--
-- Compaction works in a following manner. Total 8 bytes: first 4 bytes are for
-- SlotNo (s0-s3), followed by 2 bytes for CertIx (c0-c1) and 2 more bytes for TxIx (t0-t1).
--
-- @@@
--
-- ┏━━┯━━┯━━┯━━┯━━┯━━┯━━┯━━┓
-- ┃s3 s2 s1 s0┊c1 c0┊t1 t0┃
-- ┗━━┷━━┷━━┷━━┷━━┷━━┷━━┷━━┛
--
-- @@@
-- newtype Ptr = PtrCompact Word64

-- | Pointer to a slot number, transaction index and an index in certificate
-- list.
data Ptr = Ptr !SlotNo !TxIx !CertIx
  deriving (Eq, Ord, Generic, NFData, NoThunks)

instance EncCBOR Ptr where
  encCBOR (Ptr slotNo txIx certIx) = encCBOR slotNo <> encCBOR txIx <> encCBOR certIx

instance DecCBOR Ptr where
  decCBOR = Ptr <$> decCBOR <*> decCBOR <*> decCBOR

instance Show Ptr where
  showsPrec n (Ptr slotNo txIx certIx)
    | n < 1 = inner
    | otherwise = ('(' :) . inner . (")" ++)
    where
      inner =
        ("Ptr (" ++)
          . shows slotNo
          . (") (" ++)
          . shows txIx
          . (") (" ++)
          . shows certIx
          . (')' :)

{- TODO: Uncomment this once Mainnet is ready for Ptr optimization.

-- | With this pattern synonym we can recover actual values from compacted version of `Ptr`.
pattern Ptr :: SlotNo -> TxIx -> CertIx -> Ptr
pattern Ptr slotNo txIx certIx <-
  (viewPtr -> (slotNo, txIx, certIx))

{-# COMPLETE Ptr #-}

-- | `Ptr` relies on compact representation for memory efficiency and therefore
-- it will return `Nothing` if `SlotNo` takes up more than 32 bits, which is
-- totally fine for at least another 100 years.
mkPtr :: SlotNo -> TxIx -> CertIx -> Maybe Ptr
mkPtr (SlotNo slotNo) (TxIx txIx) (CertIx certIx)
  | slotNo > fromIntegral (maxBound :: Word32) = Nothing
  | otherwise =
      Just
        $! PtrCompact
          ( (slotNo `shiftL` 32) .|. (fromIntegral txIx `shiftL` 16)
              .|. fromIntegral certIx
          )

viewPtr :: Ptr -> (SlotNo, TxIx, CertIx)
viewPtr (PtrCompact ptr) =
  (SlotNo (ptr `shiftR` 32), TxIx (fromIntegral (ptr `shiftR` 16)), CertIx (fromIntegral ptr))
-}

ptrSlotNo :: Ptr -> SlotNo
ptrSlotNo (Ptr sn _ _) = sn

ptrTxIx :: Ptr -> TxIx
ptrTxIx (Ptr _ txIx _) = txIx

ptrCertIx :: Ptr -> CertIx
ptrCertIx (Ptr _ _ cIx) = cIx

instance (Typeable kr, Crypto c) => EncCBOR (Credential kr c) where
  encCBOR = \case
    KeyHashObj kh -> Plain.encodeListLen 2 <> encCBOR (0 :: Word8) <> encCBOR kh
    ScriptHashObj hs -> Plain.encodeListLen 2 <> encCBOR (1 :: Word8) <> encCBOR hs

instance (Typeable kr, Crypto c) => ToCBOR (Credential kr c)

instance (Typeable kr, Crypto c) => DecCBOR (Credential kr c) where
  decCBOR = Plain.decodeRecordSum "Credential" $
    \case
      0 -> do
        x <- decCBOR
        pure (2, KeyHashObj x)
      1 -> do
        x <- decCBOR
        pure (2, ScriptHashObj x)
      k -> invalidKey k

instance (Typeable kr, Crypto c) => FromCBOR (Credential kr c)

-- case mkPtr slotNo txIx certIx of
--   Nothing -> fail $ "SlotNo is too far into the future: " ++ show slotNo
--   Just ptr -> pure ptr

newtype GenesisCredential c = GenesisCredential
  { unGenesisCredential :: KeyHash 'Genesis c
  }
  deriving (Generic)
  deriving (Show) via Quiet (GenesisCredential c)

instance Ord (GenesisCredential c) where
  compare (GenesisCredential gh) (GenesisCredential gh') = compare gh gh'

instance Eq (GenesisCredential c) where
  (==) (GenesisCredential gh) (GenesisCredential gh') = gh == gh'

instance Crypto c => ToCBOR (GenesisCredential c) where
  toCBOR (GenesisCredential kh) = toCBOR kh

-- ==================================

instance ToExpr (Credential keyrole c)

instance ToExpr (StakeReference c)

instance ToExpr Ptr
