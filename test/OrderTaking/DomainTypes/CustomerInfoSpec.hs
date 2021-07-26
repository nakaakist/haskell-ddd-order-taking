{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.DomainTypes.CustomerInfoSpec
  ( spec
  ) where

import           Control.Lens                   ( (.~) )
import           Data.Either                    ( isLeft )
import           Data.Function                  ( (&) )
import           Data.Generics.Labels           ( )
import           Data.Text                      ( pack )
import qualified OrderTaking.DomainTypes.CustomerInfo.EmailAddress
                                               as EmailAddress
import qualified OrderTaking.DomainTypes.CustomerInfo.PersonalName
                                               as PersonalName
import           Test.Hspec

spec :: Spec
spec = do
  describe "EmailAddress" $ do
    it "should success to create toshincompos@gmail.com" $ do
      let Right e = EmailAddress.create "toshincompos@gmail.com"
      EmailAddress.value e `shouldBe` "toshincompos@gmail.com"

    it "should fail to create hoge" $ do
      isLeft (EmailAddress.create "hoge") `shouldBe` True

  describe "PersonalName" $ do
    let nameParams =
          PersonalName.Params { firstName = "nariyuki", lastName = "saito" }
    it "should success to create name with length up to 50" $ do
      let Right name = PersonalName.create nameParams
      PersonalName.value name `shouldBe` nameParams

    let longName = pack $ replicate 51 'a'
    it "should fail to create name with length more than 50" $ do
      let longFirstNameParams = nameParams & #firstName .~ longName
      isLeft (PersonalName.create longFirstNameParams) `shouldBe` True

    it "should fail to create name with length more than 50" $ do
      let longLastNameParams = nameParams & #lastName .~ longName
      isLeft (PersonalName.create longLastNameParams) `shouldBe` True

