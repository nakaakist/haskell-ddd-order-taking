{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.DomainTypes.BillingAmountSpec
  ( spec
  ) where

import qualified OrderTaking.DomainTypes.BillingAmount
                                               as BillingAmount
import qualified OrderTaking.DomainTypes.Price as Price
import           Test.Hspec

spec :: Spec
spec = do
  describe "BillingAmount" $ do
    it "should success to create BillingAmount between 1 to 10000" $ do
      let Right a = BillingAmount.create 1000.0
      BillingAmount.value a `shouldBe` 1000.0

    it "should fail to create invalid BillingAmount" $ do
      BillingAmount.create (-1.0) `shouldBe` Left
        "billing amount -1.0 is invalid. must be between 0.0 and 10000.0"

    it "should success to sum price 10 x 10" $ do
      let Right amount =
            Price.create 10.0 >>= BillingAmount.sumPrices . replicate 10
      BillingAmount.value amount `shouldBe` 100.0

    it "should fail to sum price 10 x 10000" $ do
      let result =
            Price.create 10.0 >>= BillingAmount.sumPrices . replicate 10000
      result `shouldBe` Left
        "billing amount 100000.0 is invalid. must be between 0.0 and 10000.0"
