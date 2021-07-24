{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Workflows.PlaceOrderSpec (spec) where

import Data.Either (fromRight, isRight)
import OrderTaking.Shared.DomainError (DomainError)
import OrderTaking.Shared.EitherIO (EitherIO, fromList, liftEither, liftIO, runEitherIO)
import qualified OrderTaking.Types.Address.Address as Address
import qualified OrderTaking.Types.BillingAmount as BillingAmount
import qualified OrderTaking.Types.Price as Price
import qualified OrderTaking.Types.ProductCode.ProductCode as ProductCode
import qualified OrderTaking.Workflows.PlaceOrder.PriceOrder as PriceOrder
import qualified OrderTaking.Workflows.PlaceOrder.ValidateOrder as ValidateOrder
import Test.Hspec

type Exists = Bool

mockCheckProductCode :: Exists -> ValidateOrder.CheckProductCodeExists
mockCheckProductCode e _ = e

mockCheckAddress :: Exists -> ValidateOrder.CheckAddressExists
mockCheckAddress e address
  | e = return address
  | otherwise = liftEither $ Left "address does not exist"

mockGetProductPrice :: Double -> PriceOrder.GetProductPrice
mockGetProductPrice p _ = Price.create p

unvalidatedCustomerInfo =
  ValidateOrder.UnvalidatedCustomerInfo
    { ValidateOrder.unvalidatedFirstName = "nariyuki",
      ValidateOrder.unvalidatedLastName = "saito",
      ValidateOrder.unvalidatedEmailAddress = "toshincompos@gmail.com"
    }

unvalidatedAddress =
  ValidateOrder.UnvalidatedAddress
    { ValidateOrder.unvalidatedAddressLine1 = "hoge street",
      ValidateOrder.unvalidatedAddressLine2 = "",
      ValidateOrder.unvalidatedAddressLine3 = "",
      ValidateOrder.unvalidatedAddressLine4 = "",
      ValidateOrder.unvalidatedCity = "Maebashi",
      ValidateOrder.unvalidatedZipCode = "12345"
    }

unvalidatedOrderLine =
  ValidateOrder.UnvalidatedOrderLine
    { ValidateOrder.unvalidatedOrderLineId = "1",
      ValidateOrder.unvalidatedProductCode = "W1234",
      ValidateOrder.unvalidatedQuantity = 10.0
    }

unvalidatedOrder =
  ValidateOrder.UnvalidatedOrder
    { ValidateOrder.unvalidatedOrderId = "hoge",
      ValidateOrder.unvalidatedCustomerInfo = unvalidatedCustomerInfo,
      ValidateOrder.unvalidatedShippingAddress = unvalidatedAddress,
      ValidateOrder.unvalidatedBillingAddress = unvalidatedAddress,
      ValidateOrder.unvalidatedOrderLines = replicate 2 unvalidatedOrderLine
    }

spec :: Spec
spec = do
  describe "ValidateOrder" $ do
    it "should pass valid order" $ do
      let result = ValidateOrder.validateOrder (mockCheckProductCode True) (mockCheckAddress True) unvalidatedOrder
      isRight <$> runEitherIO result `shouldReturn` True

    it "should block order if product code does not exist" $ do
      let result = ValidateOrder.validateOrder (mockCheckProductCode False) (mockCheckAddress True) unvalidatedOrder
      isRight <$> runEitherIO result `shouldReturn` False

    it "should block order if address does not exist" $ do
      let result = ValidateOrder.validateOrder (mockCheckProductCode True) (mockCheckAddress False) unvalidatedOrder
      isRight <$> runEitherIO result `shouldReturn` False

    it "should block order if parameter format in order line is invalid" $ do
      let invalidOrderLine =
            ValidateOrder.UnvalidatedOrderLine
              { ValidateOrder.unvalidatedOrderLineId = "1",
                ValidateOrder.unvalidatedProductCode = "hoge", -- invalid code
                ValidateOrder.unvalidatedQuantity = 10.0
              }
      let invalidOrder =
            ValidateOrder.UnvalidatedOrder
              { ValidateOrder.unvalidatedOrderId = "hoge",
                ValidateOrder.unvalidatedCustomerInfo = unvalidatedCustomerInfo,
                ValidateOrder.unvalidatedShippingAddress = unvalidatedAddress,
                ValidateOrder.unvalidatedBillingAddress = unvalidatedAddress,
                ValidateOrder.unvalidatedOrderLines = [invalidOrderLine]
              }
      let result = ValidateOrder.validateOrder (mockCheckProductCode True) (mockCheckAddress True) invalidOrder
      isRight <$> runEitherIO result `shouldReturn` False

  describe "PriceOrder" $ do
    let itemPrice = 10.0
    let validationResult = ValidateOrder.validateOrder (mockCheckProductCode True) (mockCheckAddress True) unvalidatedOrder
    let priceResult = validationResult >>= PriceOrder.priceOrder (mockGetProductPrice itemPrice)

    it "should calculate line price correctly" $ do
      let orderLines = PriceOrder.orderLines <$> priceResult
      let orderLine1 = head <$> orderLines
      (Price.value . PriceOrder.linePrice . fromRight undefined <$> runEitherIO orderLine1) `shouldReturn` 100.0

    it "should calculate total amount correctly" $ do
      (BillingAmount.value . PriceOrder.amountToBill . fromRight undefined <$> runEitherIO priceResult) `shouldReturn` 200.0