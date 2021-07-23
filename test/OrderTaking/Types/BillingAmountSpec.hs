{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.BillingAmountSpec (spec) where

import qualified OrderTaking.Types.BillingAmount as BillingAmount
import Test.Hspec

spec :: Spec
spec = do
  describe "BillingAmount" $ do
    it "should success to create BillingAmount between 1 to 10000" $ do
      let Right a = BillingAmount.create 1000.0
      BillingAmount.value a `shouldBe` 1000.0

    it "should fail to create invalid BillingAmount" $ do
      BillingAmount.create (-1.0) `shouldBe` Left "billing amount -1.0 is invalid. must be between 0.0 and 10000.0"