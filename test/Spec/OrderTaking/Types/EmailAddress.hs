{-# LANGUAGE OverloadedStrings #-}

import OrderTaking.Types.EmailAddress as EmailAddress
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "EmailAddress" $ do
    it "should success to create toshincompos@gmail.com" $ do
      let Right e = EmailAddress.create "toshincompos@gmail.com"
      EmailAddress.value e `shouldBe` "toshincompos@gmail.com"

    it "should fail to create hoge" $ do
      EmailAddress.create "hoge" `shouldBe` Left "email address hoge is invalid. at sign > @: not enough input"