{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}

-- | Generation of genesis data for testing or development.
--
-- This includes the genesis block and all required private keys (root keys,
-- keys for the initial UTxO etc).
--
-- This can never be used for a production system since all stake holder keys
-- must be generated by each stake holder privately, whereas for testing it
-- is fine to generate all the keys in one place.
module Cardano.Chain.Genesis.Generate
  ( GeneratedSecrets (..),
    gsSigningKeys,
    gsSigningKeysPoor,
    PoorSecret (..),
    generateGenesisData,
    generateGenesisDataWithEntropy,
    generateGenesisConfig,
    generateGenesisConfigWithEntropy,
    GenesisDataGenerationError (..),
  )
where

import Cardano.Chain.Common
  ( Address,
    Lovelace,
    LovelaceError,
    addLovelace,
    divLovelace,
    hashKey,
    makeVerKeyAddress,
    mkKnownLovelace,
    modLovelace,
    scaleLovelace,
    scaleLovelaceRational,
    subLovelace,
    sumLovelace,
  )
import Cardano.Chain.Common.NetworkMagic (makeNetworkMagic)
import qualified Cardano.Chain.Delegation.Certificate as Delegation
import Cardano.Chain.Genesis.AvvmBalances (GenesisAvvmBalances (..))
import Cardano.Chain.Genesis.Config (Config (..))
import Cardano.Chain.Genesis.Data (GenesisData (..))
import Cardano.Chain.Genesis.Delegation
  ( GenesisDelegation (..),
    GenesisDelegationError,
    mkGenesisDelegation,
  )
import Cardano.Chain.Genesis.Hash (GenesisHash (..))
import Cardano.Chain.Genesis.Initializer
  ( FakeAvvmOptions (..),
    GenesisInitializer (..),
    TestnetBalanceOptions (..),
  )
import Cardano.Chain.Genesis.KeyHashes (GenesisKeyHashes (..))
import Cardano.Chain.Genesis.NonAvvmBalances (GenesisNonAvvmBalances (..))
import Cardano.Chain.Genesis.Spec (GenesisSpec (..))
import Cardano.Chain.UTxO.UTxOConfiguration (defaultUTxOConfiguration)
import Cardano.Crypto as Crypto
  ( RedeemSigningKey,
    SigningKey,
    getProtocolMagicId,
    getRequiresNetworkMagic,
    keyGen,
    noPassSafeSigner,
    redeemKeyGen,
    redeemToVerification,
    runSecureRandom,
    serializeCborHash,
    toCompactRedeemVerificationKey,
    toVerification,
  )
import Cardano.Prelude
import qualified Crypto.Random as Crypto (MonadRandom)
import Data.Coerce (coerce)
import qualified Data.Map.Strict as M
import qualified Data.Set as Set
import Data.Time (UTCTime)
import Formatting (bprint, build, int, stext)
import qualified Formatting.Buildable as B
import NoThunks.Class (NoThunks (..))

-- | Poor node secret
type PoorSecret :: Type
newtype PoorSecret = PoorSecret {poorSecretToKey :: SigningKey}
  deriving (Generic, NoThunks)

-- | Valuable secrets which can unlock genesis data.
type GeneratedSecrets :: Type
data GeneratedSecrets = GeneratedSecrets
  { -- | Secret keys which issued heavyweight delegation certificates
    -- in genesis data. If genesis heavyweight delegation isn't used,
    -- this list is empty.
    gsDlgIssuersSecrets :: ![SigningKey],
    -- | All secrets of rich nodes.
    gsRichSecrets :: ![SigningKey],
    -- | Keys for HD addresses of poor nodes.
    gsPoorSecrets :: ![PoorSecret],
    -- | Fake avvm secrets.
    gsFakeAvvmSecrets :: ![RedeemSigningKey]
  }
  deriving (Generic, NoThunks)

gsSigningKeys :: GeneratedSecrets -> [SigningKey]
gsSigningKeys gs = gsRichSecrets gs <> gsSigningKeysPoor gs

gsSigningKeysPoor :: GeneratedSecrets -> [SigningKey]
gsSigningKeysPoor = map poorSecretToKey . gsPoorSecrets

type GenesisDataGenerationError :: Type
data GenesisDataGenerationError
  = GenesisDataAddressBalanceMismatch Text Int Int
  | GenesisDataGenerationDelegationError GenesisDelegationError
  | GenesisDataGenerationDistributionMismatch Lovelace Lovelace
  | GenesisDataGenerationLovelaceError LovelaceError
  | GenesisDataGenerationPassPhraseMismatch
  | GenesisDataGenerationRedeemKeyGen
  deriving (Eq, Show)

instance B.Buildable GenesisDataGenerationError where
  build = \case
    GenesisDataAddressBalanceMismatch distr addresses balances ->
      bprint
        ( "GenesisData address balance mismatch, Distribution: "
            . stext
            . " Addresses list length: "
            . int
            . " Balances list length: "
            . int
        )
        distr
        addresses
        balances
    GenesisDataGenerationDelegationError genesisDelegError ->
      bprint
        ( "GenesisDataGenerationDelegationError: "
            . build
        )
        genesisDelegError
    GenesisDataGenerationDistributionMismatch testBalance totalBalance ->
      bprint
        ( "GenesisDataGenerationDistributionMismatch: Test balance: "
            . build
            . " Total balance: "
            . build
        )
        testBalance
        totalBalance
    GenesisDataGenerationLovelaceError lovelaceErr ->
      bprint
        ( "GenesisDataGenerationLovelaceError: "
            . build
        )
        lovelaceErr
    GenesisDataGenerationPassPhraseMismatch ->
      bprint "GenesisDataGenerationPassPhraseMismatch"
    GenesisDataGenerationRedeemKeyGen ->
      bprint "GenesisDataGenerationRedeemKeyGen"

-- | Generate a genesis 'GenesisData' and 'GeneratedSecrets' from a
-- 'GenesisSpec'. This is used only for tests blockhains. For a real blockcain
-- you must use the external key generation tool so that each stakeholder can
-- generate their keys privately.
generateGenesisData ::
  UTCTime ->
  GenesisSpec ->
  ExceptT GenesisDataGenerationError IO (GenesisData, GeneratedSecrets)
generateGenesisData startTime genesisSpec =
  -- Use a sensible choice of random entropy for key generation, which then
  -- requires that the whole thing is actually in IO.
  mapExceptT Crypto.runSecureRandom $
    generateGenesisDataWithEntropy startTime genesisSpec

-- | A version of 'generateGenesisData' parametrised over 'Crypto.MonadRandom'.
-- For testing purposes this allows using a completely pure deterministic
-- entropy source, rather than a cryptographically secure entropy source.
generateGenesisDataWithEntropy ::
  Crypto.MonadRandom m =>
  UTCTime ->
  GenesisSpec ->
  ExceptT GenesisDataGenerationError m (GenesisData, GeneratedSecrets)
generateGenesisDataWithEntropy startTime genesisSpec = do
  let pm = gsProtocolMagic genesisSpec
      nm = makeNetworkMagic pm
      gi = gsInitializer genesisSpec
      fao = giFakeAvvmBalance gi
      tbo = giTestBalance gi

  -- Generate all the private keys
  generatedSecrets <- lift $ generateSecrets gi
  let dlgIssuersSecrets = gsDlgIssuersSecrets generatedSecrets
      richSecrets = gsRichSecrets generatedSecrets
      poorSecrets = gsPoorSecrets generatedSecrets

  -- Genesis Keys
  let genesisSecrets =
        if giUseHeavyDlg gi then dlgIssuersSecrets else richSecrets

      genesisKeyHashes :: GenesisKeyHashes
      genesisKeyHashes =
        GenesisKeyHashes
          . Set.fromList
          $ hashKey
            . toVerification
            <$> genesisSecrets

  -- Heavyweight delegation.
  -- genesisDlgList is empty if giUseHeavyDlg = False
  let genesisDlgList :: [Delegation.Certificate]
      genesisDlgList =
        ( \(issuerSK, delegateSK) ->
            Delegation.signCertificate
              (getProtocolMagicId pm)
              (toVerification delegateSK)
              0
              (noPassSafeSigner issuerSK)
        )
          <$> zip dlgIssuersSecrets richSecrets

  genesisDlg <-
    mkGenesisDelegation
      ( M.elems (unGenesisDelegation $ gsHeavyDelegation genesisSpec)
          <> genesisDlgList
      )
      `wrapError` GenesisDataGenerationDelegationError

  -- Real AVVM Balances
  let applyAvvmBalanceFactor :: Map k Lovelace -> Map k Lovelace
      applyAvvmBalanceFactor =
        map (flip scaleLovelaceRational (giAvvmBalanceFactor gi))

      realAvvmMultiplied :: GenesisAvvmBalances
      realAvvmMultiplied =
        GenesisAvvmBalances
          . applyAvvmBalanceFactor
          . unGenesisAvvmBalances
          . gsAvvmDistr
          $ genesisSpec

  -- Fake AVVM Balances
  let fakeAvvmVerificationKeys =
        map
          (toCompactRedeemVerificationKey . redeemToVerification)
          (gsFakeAvvmSecrets generatedSecrets)
      fakeAvvmDistr =
        GenesisAvvmBalances . M.fromList $
          map
            (,faoOneBalance fao)
            fakeAvvmVerificationKeys

  -- Non AVVM balances
  ---- Addresses
  let createAddressPoor ::
        MonadError GenesisDataGenerationError m => PoorSecret -> m Address
      createAddressPoor (PoorSecret secret) =
        pure $ makeVerKeyAddress nm (toVerification secret)
  let richAddresses = map (makeVerKeyAddress nm . toVerification) richSecrets

  poorAddresses <- mapM createAddressPoor poorSecrets

  ---- Balances
  totalFakeAvvmBalance <-
    scaleLovelace (faoOneBalance fao) (faoCount fao)
      `wrapError` GenesisDataGenerationLovelaceError

  -- Compute total balance to generate
  avvmSum <-
    sumLovelace (unGenesisAvvmBalances realAvvmMultiplied)
      `wrapError` GenesisDataGenerationLovelaceError
  maxTnBalance <-
    subLovelace maxBound avvmSum `wrapError` GenesisDataGenerationLovelaceError
  let tnBalance = min maxTnBalance (tboTotalBalance tbo)

  let safeZip ::
        MonadError GenesisDataGenerationError m =>
        Text ->
        [a] ->
        [b] ->
        m [(a, b)]
      safeZip s a b =
        if length a /= length b
          then
            throwError $
              GenesisDataAddressBalanceMismatch s (length a) (length b)
          else pure $ zip a b

  nonAvvmBalance <-
    subLovelace tnBalance totalFakeAvvmBalance
      `wrapError` GenesisDataGenerationLovelaceError

  (richBals, poorBals) <- genTestnetDistribution tbo nonAvvmBalance

  richDistr <- safeZip "richDistr" richAddresses richBals
  poorDistr <- safeZip "poorDistr" poorAddresses poorBals

  let nonAvvmDistr = GenesisNonAvvmBalances . M.fromList $ richDistr ++ poorDistr

  let genesisData =
        GenesisData
          { gdGenesisKeyHashes = genesisKeyHashes,
            gdHeavyDelegation = genesisDlg,
            gdStartTime = startTime,
            gdNonAvvmBalances = nonAvvmDistr,
            gdProtocolParameters = gsProtocolParameters genesisSpec,
            gdK = gsK genesisSpec,
            gdProtocolMagicId = getProtocolMagicId pm,
            gdAvvmDistr = fakeAvvmDistr <> realAvvmMultiplied
          }

  pure (genesisData, generatedSecrets)

generateSecrets ::
  Crypto.MonadRandom m =>
  GenesisInitializer ->
  m GeneratedSecrets
generateSecrets gi = do
  -- Generate fake AVVM secrets
  fakeAvvmSecrets <-
    replicateM
      (fromIntegral $ faoCount fao)
      (snd <$> redeemKeyGen)

  -- Generate secret keys
  dlgIssuersSecrets <-
    if giUseHeavyDlg gi
      then replicateRich (snd <$> keyGen)
      else pure []

  richSecrets <- replicateRich (snd <$> keyGen)

  poorSecrets <- replicateM (fromIntegral $ tboPoors tbo) genPoorSecret

  pure $
    GeneratedSecrets
      { gsDlgIssuersSecrets = dlgIssuersSecrets,
        gsRichSecrets = richSecrets,
        gsPoorSecrets = poorSecrets,
        gsFakeAvvmSecrets = fakeAvvmSecrets
      }
  where
    fao = giFakeAvvmBalance gi
    tbo = giTestBalance gi

    replicateRich :: Applicative m => m a -> m [a]
    replicateRich = replicateM (fromIntegral $ tboRichmen tbo)

    genPoorSecret :: Crypto.MonadRandom m => m PoorSecret
    genPoorSecret = PoorSecret . snd <$> keyGen

----------------------------------------------------------------------------
-- Generating a Genesis Config
----------------------------------------------------------------------------

-- | Generate a genesis 'Config' from a 'GenesisSpec'. This is used only for
-- tests. For the real node we always generate an external JSON genesis file.
generateGenesisConfig ::
  UTCTime ->
  GenesisSpec ->
  ExceptT GenesisDataGenerationError IO (Config, GeneratedSecrets)
generateGenesisConfig startTime genesisSpec =
  -- Use a sensible choice of random entropy for key generation, which then
  -- requires that the whole thing is actually in IO.
  mapExceptT Crypto.runSecureRandom $
    generateGenesisConfigWithEntropy startTime genesisSpec

-- | A version of 'generateGenesisConfig' parametrised over 'Crypto.MonadRandom'.
-- For testing purposes this allows using a completely pure deterministic
-- entropy source, rather than a cryptographically secure entropy source.
generateGenesisConfigWithEntropy ::
  Crypto.MonadRandom m =>
  UTCTime ->
  GenesisSpec ->
  ExceptT GenesisDataGenerationError m (Config, GeneratedSecrets)
generateGenesisConfigWithEntropy startTime genesisSpec = do
  (genesisData, generatedSecrets) <-
    generateGenesisDataWithEntropy startTime genesisSpec

  let config =
        Config
          { configGenesisData = genesisData,
            configGenesisHash = genesisHash,
            configReqNetMagic =
              getRequiresNetworkMagic
                (gsProtocolMagic genesisSpec),
            configUTxOConfiguration = defaultUTxOConfiguration
          }
  return (config, generatedSecrets)
  where
    -- Anything will do for the genesis hash. A hash of "patak" was used before,
    -- and so it remains. Here lies the last of the Serokell code. RIP.
    genesisHash = GenesisHash $ coerce $ serializeCborHash ("patak" :: Text)

----------------------------------------------------------------------------
-- Internal helpers
----------------------------------------------------------------------------

-- | Generates balance distribution for testnet
genTestnetDistribution ::
  MonadError GenesisDataGenerationError m =>
  TestnetBalanceOptions ->
  Lovelace ->
  m ([Lovelace], [Lovelace])
genTestnetDistribution tbo testBalance = do
  (richBalances, poorBalances, totalBalance) <-
    (`wrapError` GenesisDataGenerationLovelaceError) $ do
      richmanBalance <- divLovelace desiredRichBalance tboRichmen

      richmanBalanceExtra <- modLovelace desiredRichBalance tboRichmen

      richmanBalance' <-
        if tboRichmen == 0
          then pure $ mkKnownLovelace @0
          else
            addLovelace
              richmanBalance
              ( if richmanBalanceExtra > mkKnownLovelace @0
                  then mkKnownLovelace @1
                  else mkKnownLovelace @0
              )

      totalRichBalance <- scaleLovelace richmanBalance' tboRichmen

      desiredPoorsBalance <- subLovelace testBalance totalRichBalance

      poorBalance <-
        if tboPoors == 0
          then pure $ mkKnownLovelace @0
          else divLovelace desiredPoorsBalance tboPoors

      totalPoorBalance <- scaleLovelace poorBalance tboPoors

      totalBalance <- addLovelace totalRichBalance totalPoorBalance

      pure
        ( replicate (fromIntegral tboRichmen) richmanBalance',
          replicate (fromIntegral tboPoors) poorBalance,
          totalBalance
        )

  if totalBalance <= testBalance
    then pure (richBalances, poorBalances)
    else
      throwError $
        GenesisDataGenerationDistributionMismatch testBalance totalBalance
  where
    TestnetBalanceOptions {tboPoors, tboRichmen} = tbo

    desiredRichBalance = scaleLovelaceRational testBalance (tboRichmenShare tbo)
