{-# LANGUAGE OverloadedStrings #-}

import OrderTaking.Types.OrderId as OrderId
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "OrderId" $ do
    it "should success to create order ID hoge" $ do
      let Right id = OrderId.create "hoge"
      OrderId.value id `shouldBe` "hoge"