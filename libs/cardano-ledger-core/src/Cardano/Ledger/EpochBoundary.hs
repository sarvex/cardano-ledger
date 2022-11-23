{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

-- |
-- Module      : EpochBoundary
-- Description : Functions and definitions for rules at epoch boundary.
--
-- This modules implements the necessary functions for the changes that can happen at epoch boundaries.
module Cardano.Ledger.EpochBoundary
  ( Stake (..),
    sumAllStake,
    sumStakePerPool,
    SnapShot (.., SnapShot),
    SnapShots (..),
    emptySnapShot,
    emptySnapShots,
    poolStake,
    obligation,
    maxPool,
    maxPool',
    calculatePoolDistr,
    calculatePoolDistr',
    calculatePoolStake,
  )
where

import Cardano.Ledger.BaseTypes (BoundedRational (..), NonNegativeInterval)
import Cardano.Ledger.Binary
  ( FromCBOR (fromCBOR),
    FromSharedCBOR (..),
    Interns,
    ToCBOR (toCBOR),
    decodeRecordNamedT,
    encodeListLen,
    fromSharedPlusLensCBOR,
    toMemptyLens,
  )
import Cardano.Ledger.Coin
  ( Coin (..),
    CompactForm (..),
    coinToRational,
    rationalToCoinViaFloor,
  )
import Cardano.Ledger.Compactible
import Cardano.Ledger.Credential (Credential)
import qualified Cardano.Ledger.Crypto as CC (Crypto)
import Cardano.Ledger.Keys (KeyHash, KeyRole (..))
import qualified Cardano.Ledger.PoolDistr as PD
import Cardano.Ledger.PoolParams (PoolParams)
import qualified Cardano.Ledger.PoolParams as PP
import Cardano.Ledger.Val ((<+>), (<×>))
import Control.DeepSeq (NFData)
import Control.Monad.Trans (lift)
import Data.Default.Class (Default, def)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as Map
import Data.Ratio ((%))
import Data.Typeable
import Data.VMap as VMap
import GHC.Generics (Generic)
import GHC.Records (HasField, getField)
import GHC.Word (Word64)
import Lens.Micro (_1, _2)
import NoThunks.Class (AllowThunksIn (..), NoThunks (..))
import Numeric.Natural (Natural)

-- | Type of stake as map from hash key to coins associated.
newtype Stake c = Stake
  { unStake :: VMap VB VP (Credential 'Staking c) (CompactForm Coin)
  }
  deriving (Show, Eq, NFData, Generic)

deriving newtype instance Typeable c => NoThunks (Stake c)

deriving newtype instance
  CC.Crypto c => ToCBOR (Stake c)

instance CC.Crypto c => FromSharedCBOR (Stake c) where
  type Share (Stake c) = Share (VMap VB VP (Credential 'Staking c) (CompactForm Coin))
  getShare = getShare . unStake
  fromSharedCBOR = fmap Stake . fromSharedCBOR

sumAllStake :: Stake c -> Coin
sumAllStake = fromCompact . CompactCoin . VMap.foldl (\acc (CompactCoin c) -> acc + c) 0 . unStake
{-# INLINE sumAllStake #-}

-- | Get stake of one pool
poolStake ::
  KeyHash 'StakePool c ->
  VMap VB VB (Credential 'Staking c) (KeyHash 'StakePool c) ->
  Stake c ->
  Stake c
poolStake hk delegs (Stake stake) =
  -- Stake $ (eval (dom (delegs ▷ setSingleton hk) ◁ stake))
  Stake $ VMap.filter (\cred _ -> VMap.lookup cred delegs == Just hk) stake

sumStakePerPool ::
  VMap VB VB (Credential 'Staking c) (KeyHash 'StakePool c) ->
  Stake c ->
  Map (KeyHash 'StakePool c) Coin
sumStakePerPool delegs (Stake stake) = VMap.foldlWithKey accum Map.empty stake
  where
    accum !acc cred compactCoin =
      case VMap.lookup cred delegs of
        Nothing -> acc
        Just kh -> Map.insertWith (<+>) kh (fromCompact compactCoin) acc

-- | Calculate total possible refunds.
obligation ::
  forall c pp t.
  ( HasField "_keyDeposit" pp Coin,
    HasField "_poolDeposit" pp Coin,
    Foldable (t (Credential 'Staking c))
  ) =>
  pp ->
  t (Credential 'Staking c) Coin ->
  Map (KeyHash 'StakePool c) (PoolParams c) ->
  Coin
obligation pp rewards stakePools =
  (length rewards <×> getField @"_keyDeposit" pp)
    <+> (length stakePools <×> getField @"_poolDeposit" pp)

-- | Calculate maximal pool reward
maxPool' ::
  NonNegativeInterval ->
  Natural ->
  Coin ->
  Rational ->
  Rational ->
  Coin
maxPool' a0 nOpt r sigma pR = rationalToCoinViaFloor $ factor1 * factor2
  where
    z0 = 1 % fromIntegral nOpt
    sigma' = min sigma z0
    p' = min pR z0
    factor1 = coinToRational r / (1 + unboundRational a0)
    factor2 = sigma' + p' * unboundRational a0 * factor3
    factor3 = (sigma' - p' * factor4) / z0
    factor4 = (z0 - sigma') / z0

-- | Version of maxPool' that extracts a0 and nOpt from a PParam with the right HasField instances
maxPool ::
  (HasField "_a0" pp NonNegativeInterval, HasField "_nOpt" pp Natural) =>
  pp ->
  Coin ->
  Rational ->
  Rational ->
  Coin
maxPool pc r sigma pR = maxPool' a0 nOpt r sigma pR
  where
    a0 = getField @"_a0" pc
    nOpt = getField @"_nOpt" pc

pattern SnapShot ::
  forall c.
  Stake c ->
  VMap VB VB (Credential 'Staking c) (KeyHash 'StakePool c) ->
  VMap VB VB (KeyHash 'StakePool c) (PoolParams c) ->
  SnapShot c
pattern SnapShot {ssStake, ssDelegations, ssPoolParams} <-
  SnapShotRaw
    { ssStake,
      ssDelegations,
      ssPoolParams,
      ssStakePoolDistr = _ssStakePoolDistr
    }
  where
    SnapShot
      ssStake
      ssDelegations
      ssPoolParams =
        SnapShotRaw
          { ssStake,
            ssDelegations,
            ssPoolParams,
            ssStakePoolDistr = calculatePoolDistr ssStake ssDelegations ssPoolParams
          }

{-# COMPLETE SnapShot #-}

-- | Snapshot of the stake distribution.
data SnapShot c = SnapShotRaw
  { ssStake :: !(Stake c),
    ssDelegations :: !(VMap VB VB (Credential 'Staking c) (KeyHash 'StakePool c)),
    ssPoolParams :: !(VMap VB VB (KeyHash 'StakePool c) (PoolParams c)),
    ssStakePoolDistr :: PD.PoolDistr c -- Lazy on purpose
  }
  deriving (Show, Eq, Generic)
  deriving (NoThunks) via AllowThunksIn '["ssStakePoolDistr"] (SnapShot c)

instance NFData (SnapShot c)

instance
  CC.Crypto c =>
  ToCBOR (SnapShot c)
  where
  toCBOR
    SnapShotRaw
      { ssStake = s,
        ssDelegations = d,
        ssPoolParams = p
      } =
      encodeListLen 3
        <> toCBOR s
        <> toCBOR d
        <> toCBOR p

instance CC.Crypto c => FromSharedCBOR (SnapShot c) where
  type
    Share (SnapShot c) =
      (Interns (Credential 'Staking c), Interns (KeyHash 'StakePool c))
  fromSharedPlusCBOR = decodeRecordNamedT "SnapShot" (const 3) $ do
    ssStake <- fromSharedPlusLensCBOR _1
    ssDelegations <- fromSharedPlusCBOR
    ssPoolParams <- fromSharedPlusLensCBOR (toMemptyLens _1 _2)
    pure SnapShot {ssStake, ssDelegations, ssPoolParams}

-- | Snapshots of the stake distribution.
data SnapShots c = SnapShots
  { ssStakeMark :: SnapShot c, -- Lazy on purpose
    ssStakeSet :: !(SnapShot c),
    ssStakeGo :: !(SnapShot c),
    ssFee :: !Coin
  }
  deriving (Show, Eq, Generic)
  deriving (NoThunks) via AllowThunksIn '["ssStakeMark"] (SnapShots c)

instance NFData (SnapShots c)

instance
  CC.Crypto c =>
  ToCBOR (SnapShots c)
  where
  toCBOR (SnapShots {ssStakeMark, ssStakeSet, ssStakeGo, ssFee}) =
    encodeListLen 4
      <> toCBOR ssStakeMark
      <> toCBOR ssStakeSet
      <> toCBOR ssStakeGo
      <> toCBOR ssFee

instance CC.Crypto c => FromSharedCBOR (SnapShots c) where
  type Share (SnapShots c) = Share (SnapShot c)
  fromSharedPlusCBOR = decodeRecordNamedT "SnapShots" (const 4) $ do
    !ssStakeMark <- fromSharedPlusCBOR
    ssStakeSet <- fromSharedPlusCBOR
    ssStakeGo <- fromSharedPlusCBOR
    ssFee <- lift fromCBOR
    pure SnapShots {ssStakeMark, ssStakeSet, ssStakeGo, ssFee}

instance Default (SnapShots c) where
  def = emptySnapShots

emptySnapShot :: SnapShot c
emptySnapShot = SnapShot (Stake VMap.empty) VMap.empty VMap.empty

emptySnapShots :: SnapShots c
emptySnapShots = SnapShots emptySnapShot emptySnapShot emptySnapShot (Coin 0)

calculatePoolDistr ::
  Stake c ->
  VMap VB VB (Credential 'Staking c) (KeyHash 'StakePool c) ->
  VMap VB VB (KeyHash 'StakePool c) (PoolParams c) ->
  PD.PoolDistr c
calculatePoolDistr = calculatePoolDistr' (const True)

calculatePoolDistr' ::
  forall c.
  (KeyHash 'StakePool c -> Bool) ->
  Stake c ->
  VMap VB VB (Credential 'Staking c) (KeyHash 'StakePool c) ->
  VMap VB VB (KeyHash 'StakePool c) (PoolParams c) ->
  PD.PoolDistr c
calculatePoolDistr' includeHash stake delegs poolParams =
  let Coin total = sumAllStake stake
      -- total could be zero (in particular when shrinking)
      nonZeroTotal :: Integer
      nonZeroTotal = if total == 0 then 1 else total
      poolStakeMap :: Map.Map (KeyHash 'StakePool c) Word64
      poolStakeMap = calculatePoolStake includeHash delegs stake
   in PD.PoolDistr $
        Map.intersectionWith
          (\word64 poolparam -> PD.IndividualPoolStake (toInteger word64 % nonZeroTotal) (PP.ppVrf poolparam))
          poolStakeMap
          (VMap.toMap poolParams)

-- | Sum up the Coin (as CompactForm Coin = Word64) for each StakePool
calculatePoolStake ::
  (KeyHash 'StakePool c -> Bool) ->
  VMap VB VB (Credential 'Staking c) (KeyHash 'StakePool c) ->
  Stake c ->
  Map.Map (KeyHash 'StakePool c) Word64
calculatePoolStake includeHash delegs stake = VMap.foldlWithKey accum Map.empty delegs
  where
    accum ans cred keyHash =
      if includeHash keyHash
        then case VMap.lookup cred (unStake stake) of
          Nothing -> ans
          Just (CompactCoin c) -> Map.insertWith (+) keyHash c ans
        else ans