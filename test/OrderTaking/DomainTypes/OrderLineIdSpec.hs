{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.DomainTypes.OrderLineIdSpec
  ( spec
  ) where

import qualified OrderTaking.DomainTypes.OrderLineId
                                               as OrderLineId
import           Test.Hspec

spec :: Spec
spec = do
  describe "OrderLineId" $ do
    it "should success to create order line ID hoge" $ do
      let Right orderLineId = OrderLineId.create "hoge"
      OrderLineId.value orderLineId `shouldBe` "hoge"
