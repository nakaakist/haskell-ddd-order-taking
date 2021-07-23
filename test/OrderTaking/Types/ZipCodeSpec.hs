{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.ZipCodeSpec (spec) where

import OrderTaking.Types.ZipCode as ZipCode
import Test.Hspec

spec :: Spec
spec = do
  describe "ZipCode" $ do
    it "should success to create 12345" $ do
      let Right e = ZipCode.create "12345"
      ZipCode.value e `shouldBe` "12345"

    it "should fail to create hoge" $ do
      ZipCode.create "hoge" `shouldBe` Left "zip code hoge is invalid. must be 5 digits number"

    it "should fail to create 1234" $ do
      ZipCode.create "1234" `shouldBe` Left "zip code 1234 is invalid. must be 5 digits number"