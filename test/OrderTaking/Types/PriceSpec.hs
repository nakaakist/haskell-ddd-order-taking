{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.PriceSpec
  ( spec
  ) where

import qualified OrderTaking.Types.Price       as Price
import           Test.Hspec

spec :: Spec
spec = do
  describe "Price" $ do
    it "should success to create price between 1 to 1000" $ do
      let Right price = Price.create 10.0
      Price.value price `shouldBe` 10.0

    it "should fail to create invalid price" $ do
      Price.create (-1.0) `shouldBe` Left
        "price -1.0 is invalid. must be between 0.0 and 1000.0"

    it "should success to multiply price 10 x 10" $ do
      let Right price = Price.create 10.0 >>= Price.multiply 10.0
      Price.value price `shouldBe` 100.0

    it "should fail to multiply price 10 x 1000" $ do
      (Price.create 10.0 >>= Price.multiply 1000.0) `shouldBe` Left
        "price 10000.0 is invalid. must be between 0.0 and 1000.0"
