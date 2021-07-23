{-# LANGUAGE OverloadedStrings #-}

import OrderTaking.Types.OrderLineId as OrderLineId
import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "OrderLineId" $ do
    it "should success to create order line ID hoge" $ do
      let Right id = OrderLineId.create "hoge"
      OrderLineId.value id `shouldBe` "hoge"