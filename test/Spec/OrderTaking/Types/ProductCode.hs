{-# LANGUAGE OverloadedStrings #-}

import OrderTaking.Types.ProductCode as ProductCode
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "ProductCode" $ do
    it "should success to create widget code W1234" $ do
      let Right code = ProductCode.create "W1234"
      ProductCode.value code `shouldBe` "W1234"

    it "should success to create gizmo code G123" $ do
      let Right code = ProductCode.create "G123"
      ProductCode.value code `shouldBe` "G123"

    it "should fail to create product code hoge" $ do
      ProductCode.create "hoge" `shouldBe` Left "product code hoge is invalid. must start with G or W"

    it "should fail to create product code W123" $ do
      ProductCode.create "W123" `shouldBe` Left "widget code W123 is invalid. must be Wxxxx"

    it "should fail to create product code G12" $ do
      ProductCode.create "G12" `shouldBe` Left "gizmo code G12 is invalid. must be Gxxx"
