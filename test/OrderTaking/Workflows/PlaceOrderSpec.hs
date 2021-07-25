{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Workflows.PlaceOrderSpec
  ( spec
  ) where

import           Control.Lens                   ( (.~)
                                                , (^.)
                                                )
import           Data.Either                    ( fromRight
                                                , isRight
                                                )
import           Data.Function
import           OrderTaking.Shared.DomainError ( DomainError )
import           OrderTaking.Shared.EitherIO    ( EitherIO
                                                , fromList
                                                , liftEither
                                                , liftIO
                                                , runEitherIO
                                                )
import qualified OrderTaking.Types.Address.Address
                                               as Address
import qualified OrderTaking.Types.BillingAmount
                                               as BillingAmount
import qualified OrderTaking.Types.Price       as Price
import qualified OrderTaking.Types.ProductCode.ProductCode
                                               as ProductCode
import qualified OrderTaking.Workflows.PlaceOrder.PriceOrder
                                               as PriceOrder
import qualified OrderTaking.Workflows.PlaceOrder.ValidateOrder
                                               as ValidateOrder
import           Test.Hspec

type Exists = Bool

mockCheckProductCode :: Exists -> ValidateOrder.CheckProductCodeExists
mockCheckProductCode e _ = e

mockCheckAddress :: Exists -> ValidateOrder.CheckAddressExists
mockCheckAddress e address
  | e         = return address
  | otherwise = liftEither $ Left "address does not exist"

mockGetProductPrice :: Double -> PriceOrder.GetProductPrice
mockGetProductPrice p _ = Price.create p

unvalidatedCustomerInfo = ValidateOrder.UnvalidatedCustomerInfo
  { firstName    = "nariyuki"
  , lastName     = "saito"
  , emailAddress = "toshincompos@gmail.com"
  }

unvalidatedAddress = ValidateOrder.UnvalidatedAddress
  { addressLine1 = "hoge street"
  , addressLine2 = ""
  , addressLine3 = ""
  , addressLine4 = ""
  , city         = "Maebashi"
  , zipCode      = "12345"
  }

unvalidatedOrderLine = ValidateOrder.UnvalidatedOrderLine
  { orderLineId = "1"
  , productCode = "W1234"
  , quantity    = 10.0
  }

unvalidatedOrder = ValidateOrder.UnvalidatedOrder
  { orderId         = "hoge"
  , customerInfo    = unvalidatedCustomerInfo
  , shippingAddress = unvalidatedAddress
  , billingAddress  = unvalidatedAddress
  , orderLines      = replicate 2 unvalidatedOrderLine
  }

spec :: Spec
spec = do
  describe "ValidateOrder" $ do
    it "should pass valid order" $ do
      let result = ValidateOrder.validateOrder (mockCheckProductCode True)
                                               (mockCheckAddress True)
                                               unvalidatedOrder
      isRight <$> runEitherIO result `shouldReturn` True

    it "should block order if product code does not exist" $ do
      let result = ValidateOrder.validateOrder (mockCheckProductCode False)
                                               (mockCheckAddress True)
                                               unvalidatedOrder
      isRight <$> runEitherIO result `shouldReturn` False

    it "should block order if address does not exist" $ do
      let result = ValidateOrder.validateOrder (mockCheckProductCode True)
                                               (mockCheckAddress False)
                                               unvalidatedOrder
      isRight <$> runEitherIO result `shouldReturn` False

    it "should block order if parameter format in order line is invalid" $ do
      let invalidOrderLine = unvalidatedOrderLine & #productCode .~ "hoge"
      let invalidOrder = unvalidatedOrder & #orderLines .~ [invalidOrderLine]
      let result = ValidateOrder.validateOrder (mockCheckProductCode True)
                                               (mockCheckAddress True)
                                               invalidOrder
      isRight <$> runEitherIO result `shouldReturn` False

  describe "PriceOrder" $ do
    let itemPrice = 10.0
    let validationResult = ValidateOrder.validateOrder
          (mockCheckProductCode True)
          (mockCheckAddress True)
          unvalidatedOrder
    let priceResult = validationResult
          >>= PriceOrder.priceOrder (mockGetProductPrice itemPrice)

    it "should calculate line price correctly" $ do
      let orderLines = PriceOrder.orderLines <$> priceResult
      let orderLine1 = head <$> orderLines
      (   Price.value
        .   PriceOrder.linePrice
        .   fromRight undefined
        <$> runEitherIO orderLine1
        )
        `shouldReturn` 100.0

    it "should calculate total amount correctly" $ do
      (   BillingAmount.value
        .   PriceOrder.amountToBill
        .   fromRight undefined
        <$> runEitherIO priceResult
        )
        `shouldReturn` 200.0
