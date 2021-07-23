{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.OrderQuantitySpec (spec) where

import qualified OrderTaking.Types.OrderQuantity.OrderQuantity as OrderQuantity
import qualified OrderTaking.Types.ProductCode.ProductCode as ProductCode
import Test.Hspec

spec :: Spec
spec = do
  describe "OrderQuantity" $ do
    let Right widgetCode = ProductCode.create "W1234"
    let Right gizmoCode = ProductCode.create "G123"

    it "should success to create unit quantity 100.5 with widget code (quantity is rounded to 100.0)" $ do
      let Right quantity = OrderQuantity.create widgetCode 100.5
      OrderQuantity.value quantity `shouldBe` 100.0

    it "should success to create kilogram quantity 10.5 with gizmo code (quantity is not rounded)" $ do
      let Right quantity = OrderQuantity.create gizmoCode 10.5
      OrderQuantity.value quantity `shouldBe` 10.5

    it "should success to create kilogram quantity 10.5 with gizmo code (quantity is not rounded)" $ do
      let Right quantity = OrderQuantity.create gizmoCode 10.5
      OrderQuantity.value quantity `shouldBe` 10.5

    it "should fail to create invalid unit quantity with widget code" $ do
      OrderQuantity.create widgetCode 0.0 `shouldBe` Left "unit quantity 0 is invalid. must be between 1 and 1000"

    it "should fail to create invalid kilogram quantity with gizmo code" $ do
      OrderQuantity.create gizmoCode 0.4 `shouldBe` Left "kilogram quantity 0.4 is invalid. must be between 0.5 and 100.0"
