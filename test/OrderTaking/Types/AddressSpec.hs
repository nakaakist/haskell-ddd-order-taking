{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.AddressSpec
  ( spec
  ) where

import           Control.Lens                   ( (.~) )
import           Data.Function                  ( (&) )
import           Data.Generics.Labels           ( )
import qualified OrderTaking.Types.Address.Address
                                               as Address
import           Test.Hspec


spec :: Spec
spec = do
  let validAddressLine = "hoge 1-1-1"
  let validCity        = "Maebashi"
  let validZipCode     = "12345"
  let addressParams = Address.Params { addressLine1 = validAddressLine
                                     , addressLine2 = validAddressLine
                                     , addressLine3 = validAddressLine
                                     , addressLine4 = validAddressLine
                                     , city         = validCity
                                     , zipCode      = validZipCode
                                     }

  describe "Address" $ do
    it "should success to create valid address" $ do
      let Right address = Address.create addressParams
      Address.value address `shouldBe` addressParams

    it "should success to create address with only address line 1" $ do
      let addressParamsWithOnlyLine1 =
            addressParams
              &  #addressLine2
              .~ ""
              &  #addressLine3
              .~ ""
              &  #addressLine4
              .~ ""
      let Right address = Address.create addressParamsWithOnlyLine1
      Address.value address `shouldBe` addressParamsWithOnlyLine1

    it "should fail to create address with zip code hoge" $ do
      let invalidAddressParams = addressParams & #zipCode .~ "hoge"
      Address.create invalidAddressParams
        `shouldBe` Left "zip code hoge is invalid. must be 5 digits number"

    it "should fail to create 1234" $ do
      let invalidAddressParams = addressParams & #zipCode .~ "1234"
      Address.create invalidAddressParams
        `shouldBe` Left "zip code 1234 is invalid. must be 5 digits number"
