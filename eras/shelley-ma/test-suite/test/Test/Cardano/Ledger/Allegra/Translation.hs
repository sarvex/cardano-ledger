{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeApplications #-}

module Test.Cardano.Ledger.Allegra.Translation (
  allegraTranslationTests,
  allegraEncodeDecodeTests,
)
where

import Cardano.Ledger.Allegra (Allegra)
import Cardano.Ledger.Binary
import Cardano.Ledger.Core
import Cardano.Ledger.Shelley (Shelley)
import qualified Cardano.Ledger.Shelley.API as S
import Test.Cardano.Ledger.Binary.RoundTrip
import Test.Cardano.Ledger.Shelley.Generator.ShelleyEraGen ()
import Test.Cardano.Ledger.Shelley.Serialisation.Generators ()
import Test.Cardano.Ledger.TranslationTools (translateEraEncoding, translateEraToCBOR)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion)
import Test.Tasty.QuickCheck (testProperty)

allegraEncodeDecodeTests :: TestTree
allegraEncodeDecodeTests =
  testGroup
    "encoded shelley types can be decoded as allegra types"
    [ testProperty
        "decoding auxiliary data"
        ( embedTripAnnExpectation @(TxAuxData Shelley) @(TxAuxData Allegra)
            (eraProtVerLow @Shelley)
            (eraProtVerLow @Allegra)
            (\_ _ -> pure ())
        )
    ]

allegraTranslationTests :: TestTree
allegraTranslationTests =
  testGroup
    "Allegra translation binary compatibiliby tests"
    [ testProperty "Tx compatibility" $
        translateEraEncoding @Allegra @S.ShelleyTx () encCBOR encCBOR
    , testProperty "ProposedPPUpdates compatibility" (testTranslation @S.ProposedPPUpdates)
    , testProperty "ShelleyPPUPState compatibility" $
        translateEraEncoding @Allegra @S.ShelleyPPUPState () encCBOR encCBOR
    , testProperty "TxOut compatibility" (testTranslation @S.ShelleyTxOut)
    , testProperty "UTxO compatibility" $
        translateEraEncoding @Allegra @S.UTxO () encCBOR encCBOR
    , testProperty "UTxOState compatibility" $
        translateEraEncoding @Allegra @S.UTxOState () encCBOR encCBOR
    , testProperty "LedgerState compatibility" $
        translateEraEncoding @Allegra @S.LedgerState () encCBOR encCBOR
    , testProperty "EpochState compatibility" $
        translateEraEncoding @Allegra @S.EpochState () encCBOR encCBOR
    , testProperty "ShelleyTxWits compatibility" $
        translateEraEncoding @Allegra @S.ShelleyTxWits () encCBOR encCBOR
    , testProperty "Update compatibility" (testTranslation @S.Update)
    ]

testTranslation ::
  forall f.
  ( ToCBOR (f Allegra)
  , ToCBOR (f Shelley)
  , TranslateEra Allegra f
  , Show (TranslationError Allegra f)
  ) =>
  f Shelley ->
  Assertion
testTranslation = translateEraToCBOR ([] :: [Allegra]) ()
