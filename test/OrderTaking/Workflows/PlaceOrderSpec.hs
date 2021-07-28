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
import           Data.Function                  ( (&) )
import           Data.Generics.Labels           ( )
import           Data.Maybe                     ( fromJust
                                                , isJust
                                                , isNothing
                                                )
import qualified OrderTaking.DomainTypes.Price as Price
import           OrderTaking.Shared.DomainError ( DomainError )
import           OrderTaking.Shared.EitherIO    ( EitherIO
                                                , liftEither
                                                , runEitherIO
                                                )
import qualified OrderTaking.Workflows.PlaceOrder.DomainTypes.Dependencies
                                               as Dependencies
import qualified OrderTaking.Workflows.PlaceOrder.DomainTypes.Inputs
                                               as Inputs
import qualified OrderTaking.Workflows.PlaceOrder.Dtos.OutputDtos
                                               as OutputDtos
import qualified OrderTaking.Workflows.PlaceOrder.PlaceOrder
                                               as PlaceOrder
import           Test.Hspec                     ( Spec
                                                , describe
                                                , it
                                                , shouldReturn
                                                )


spec :: Spec
spec = do
  let unvalidatedCustomerInfo = Inputs.UnvalidatedCustomerInfo
        { firstName    = "nariyuki"
        , lastName     = "saito"
        , emailAddress = "toshincompos@gmail.com"
        }

  let unvalidatedBillingAddress = Inputs.UnvalidatedAddress
        { addressLine1 = "hoge street"
        , addressLine2 = ""
        , addressLine3 = ""
        , addressLine4 = ""
        , city         = "billing city"
        , zipCode      = "12345"
        }

  let unvalidatedShippingAddress = Inputs.UnvalidatedAddress
        { addressLine1 = "hoge street"
        , addressLine2 = "fuga"
        , addressLine3 = "piyo"
        , addressLine4 = ""
        , city         = "shipping city"
        , zipCode      = "54321"
        }

  let unvalidatedOrderLine = Inputs.UnvalidatedOrderLine
        { orderLineId = "1"
        , productCode = "W1234"
        , quantity    = 10.0
        }

  let unvalidatedOrder = Inputs.UnvalidatedOrder
        { orderId         = "hoge"
        , customerInfo    = unvalidatedCustomerInfo
        , shippingAddress = unvalidatedShippingAddress
        , billingAddress  = unvalidatedBillingAddress
        , orderLines      = replicate 2 unvalidatedOrderLine
        }

  describe "error in validate order" $ do
    it "should error if product code does not exist" $ do
      let result = mockPlaceOrder False True 10.0 True unvalidatedOrder
      isRight <$> runEitherIO result `shouldReturn` False

    it "should error if address does not exist" $ do
      let result = mockPlaceOrder True False 10.0 True unvalidatedOrder
      isRight <$> runEitherIO result `shouldReturn` False

    it "should error if parameter format in order is invalid" $ do
      let invalidOrderLine = unvalidatedOrderLine & #productCode .~ "hoge"
      let invalidOrder = unvalidatedOrder & #orderLines .~ [invalidOrderLine]
      let result           = mockPlaceOrder True True 10.0 True invalidOrder
      isRight <$> runEitherIO result `shouldReturn` False

  describe "successful flow execution" $ do
    describe "all events are output" $ do
      let result = mockPlaceOrder True True 10.0 True unvalidatedOrder
      let events = fromRight undefined <$> runEitherIO result

      it "should output order placed event with correct amount to bill"
        $              do
                         (^. #amountToBill)
        .              (^. #orderPlaced)
        <$>            events
        `shouldReturn` 200.0

      it "should output order placed event with correct addresses" $ do
        let shippingCity =
              (^. #city) . (^. #shippingAddress) . (^. #orderPlaced) <$> events
        let billingCity =
              (^. #city) . (^. #billingAddress) . (^. #orderPlaced) <$> events
        shippingCity `shouldReturn` "shipping city"
        billingCity `shouldReturn` "billing city"

      it "should output billable order placed event with correct amount to bill"
        $              do
                         (^. #amountToBill)
        .              fromJust
        .              (^. #billableOrderPlaced)
        <$>            events
        `shouldReturn` 200.0

      it
          "should output billable order placed event with correct billing address"
        $              do
                         (^. #city)
        .              (^. #billingAddress)
        .              fromJust
        .              (^. #billableOrderPlaced)
        <$>            events
        `shouldReturn` "billing city"

      it
          "should output billable order placed event with correct billing address"
        $              do
                         (^. #city)
        .              (^. #billingAddress)
        .              fromJust
        .              (^. #billableOrderPlaced)
        <$>            events
        `shouldReturn` "billing city"

      it
          "should output order acknowledgement sent event with correct email address"
        $              do
                         (^. #emailAddress)
        .              fromJust
        .              (^. #orderAcknowledgementSent)
        <$>            events
        `shouldReturn` "toshincompos@gmail.com"

    describe "some events are not output" $ do
      it "should not output billable order placed event if billing amount is 0"
        $ do
            let result = mockPlaceOrder True True 0.0 True unvalidatedOrder
            let events = fromRight undefined <$> runEitherIO result
            isNothing . (^. #billableOrderPlaced) <$> events `shouldReturn` True
            isJust
              .              (^. #orderAcknowledgementSent)
              <$>            events
              `shouldReturn` True

      it
          "should not output order acknowledgement sent event if fail to send email"
        $ do
            let result = mockPlaceOrder True True 10.0 False unvalidatedOrder
            let events = fromRight undefined <$> runEitherIO result
            isJust . (^. #billableOrderPlaced) <$> events `shouldReturn` True
            isNothing
              .              (^. #orderAcknowledgementSent)
              <$>            events
              `shouldReturn` True




-- mock functions for test

type ProductCodeExists = Bool
type IsAddressValid = Bool
type ItemPrice = Double
type IsLetterSent = Bool

mockCheckProductCode
  :: ProductCodeExists -> Dependencies.CheckProductCodeExists
mockCheckProductCode e _ = e

mockCheckAddress :: IsAddressValid -> Dependencies.CheckAddressExists
mockCheckAddress e address
  | e         = return address
  | otherwise = liftEither $ Left "address does not exist"

mockGetProductPrice :: ItemPrice -> Dependencies.GetProductPrice
mockGetProductPrice p _ = Price.create p

mockCreateOrderAcknowledgementLetter
  :: Dependencies.CreateOrderAcknowledgmentLetter
mockCreateOrderAcknowledgementLetter _ = Dependencies.HtmlString "mock letter"

mockSendOrderAcknowledgment
  :: IsLetterSent -> Dependencies.SendOrderAcknowledgment
mockSendOrderAcknowledgment isLetterSent =
  let result = if isLetterSent then Dependencies.Sent else Dependencies.NotSent
  in  return . return result

mockPlaceOrder
  :: ProductCodeExists
  -> IsAddressValid
  -> ItemPrice
  -> IsLetterSent
  -> Inputs.UnvalidatedOrder
  -> EitherIO DomainError OutputDtos.PlaceOrderEventsDto
mockPlaceOrder productCodeExists isAddressValid itemPrice isLetterSent unvalidatedOrder
  = let result = PlaceOrder.placeOrder
          (mockCheckProductCode productCodeExists)
          (mockCheckAddress isAddressValid)
          (mockGetProductPrice itemPrice)
          mockCreateOrderAcknowledgementLetter
          (mockSendOrderAcknowledgment isLetterSent)
          unvalidatedOrder
    in  OutputDtos.placeOrderEventsDtoFromDomain <$> result
