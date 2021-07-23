{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.CustomerInfoSpec (spec) where

import Data.Either (isLeft)
import Data.Text (pack)
import qualified OrderTaking.Types.CustomerInfo.CustomerInfo as CustomerInfo
import qualified OrderTaking.Types.CustomerInfo.EmailAddress as EmailAddress
import qualified OrderTaking.Types.CustomerInfo.PersonalName as PersonalName
import Test.Hspec

spec :: Spec
spec = do
  describe "EmailAddress" $ do
    it "should success to create toshincompos@gmail.com" $ do
      let Right e = EmailAddress.create "toshincompos@gmail.com"
      EmailAddress.value e `shouldBe` "toshincompos@gmail.com"

    it "should fail to create hoge" $ do
      isLeft (EmailAddress.create "hoge") `shouldBe` True

  describe "PersonalName" $ do
    it "should success to create name with length up to 50" $ do
      let params =
            PersonalName.Params
              { PersonalName.firstName = "nariyuki",
                PersonalName.lastName = "saito"
              }
      let Right name = PersonalName.create params
      PersonalName.value name `shouldBe` params

    let longName = pack $ replicate 51 'a'
    it "should fail to create name with length more than 50" $ do
      let params =
            PersonalName.Params
              { PersonalName.firstName = longName,
                PersonalName.lastName = "saito"
              }
      isLeft (PersonalName.create params) `shouldBe` True

    it "should fail to create name with length more than 50" $ do
      let params =
            PersonalName.Params
              { PersonalName.firstName = "nariyuki",
                PersonalName.lastName = longName
              }
      isLeft (PersonalName.create params) `shouldBe` True