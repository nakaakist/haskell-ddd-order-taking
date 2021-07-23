{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.OrderIdSpec (spec) where

import qualified OrderTaking.Types.OrderId as OrderId
import Test.Hspec

spec :: Spec
spec = do
  describe "OrderId" $ do
    it "should success to create order ID hoge" $ do
      let Right id = OrderId.create "hoge"
      OrderId.value id `shouldBe` "hoge"