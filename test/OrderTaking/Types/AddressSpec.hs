{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.AddressSpec
  ( spec
  ) where

import           Data.Text                      ( Text )
import qualified OrderTaking.Types.Address.Address
                                               as Address
import qualified OrderTaking.Types.Address.ZipCode
                                               as ZipCode
import           Test.Hspec

validAddressLine = "hoge 1-1-1" :: Text

validCity = "Maebashi" :: Text

validZipCode = "12345" :: Text

spec :: Spec
spec = do
  describe "Address" $ do
    it "should success to create valid address" $ do
      let params = Address.Params { Address.addressLine1 = validAddressLine
                                  , Address.addressLine2 = validAddressLine
                                  , Address.addressLine3 = validAddressLine
                                  , Address.addressLine4 = validAddressLine
                                  , Address.city         = validCity
                                  , Address.zipCode      = validZipCode
                                  }
      let Right address = Address.create params
      Address.value address `shouldBe` params

    it "should success to create address with only address line 1" $ do
      let params = Address.Params { Address.addressLine1 = validAddressLine
                                  , Address.addressLine2 = ""
                                  , Address.addressLine3 = ""
                                  , Address.addressLine4 = ""
                                  , Address.city         = validCity
                                  , Address.zipCode      = validZipCode
                                  }
      let Right address = Address.create params
      Address.value address `shouldBe` params

    it "should fail to create address with zip code hoge" $ do
      let params = createParamsWithZipCode "hoge"
      Address.create params
        `shouldBe` Left "zip code hoge is invalid. must be 5 digits number"

    it "should fail to create 1234" $ do
      let params = createParamsWithZipCode "1234"
      Address.create params
        `shouldBe` Left "zip code 1234 is invalid. must be 5 digits number"

-- helper functions

createParamsWithZipCode :: Text -> Address.Params
createParamsWithZipCode c = Address.Params
  { Address.addressLine1 = validAddressLine
  , Address.addressLine2 = validAddressLine
  , Address.addressLine3 = validAddressLine
  , Address.addressLine4 = validAddressLine
  , Address.city         = validCity
  , Address.zipCode      = c
  }
