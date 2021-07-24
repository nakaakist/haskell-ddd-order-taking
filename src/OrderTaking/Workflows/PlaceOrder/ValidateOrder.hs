{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Workflows.PlaceOrder.ValidateOrder
  ( UnvalidatedCustomerInfo (..),
    UnvalidatedAddress (..),
    UnvalidatedOrderLine (..),
    UnvalidatedOrder (..),
    CheckProductCodeExists,
    CheckAddressExists,
    validateOrder,
  )
where

import Data.Text
import OrderTaking.Shared (DomainError, EitherIO, fromList, liftEither, liftIO, runEitherIO)
import qualified OrderTaking.Types.Address.Address as Address
import qualified OrderTaking.Types.CustomerInfo.CustomerInfo as CustomerInfo
import qualified OrderTaking.Types.CustomerInfo.EmailAddress as EmailAddress
import qualified OrderTaking.Types.CustomerInfo.PersonalName as PersonalName
import qualified OrderTaking.Types.OrderId as OrderId
import qualified OrderTaking.Types.OrderLineId as OrderLineId
import qualified OrderTaking.Types.OrderQuantity.OrderQuantity as OrderQuantity
import qualified OrderTaking.Types.ProductCode.ProductCode as ProductCode

data UnvalidatedCustomerInfo = UnvalidatedCustomerInfo
  { unvalidatedFirstName :: Text,
    unvalidatedLastName :: Text,
    unvalidatedEmailAddress :: Text
  }
  deriving (Show, Eq)

data UnvalidatedAddress = UnvalidatedAddress
  { unvalidatedAddressLine1 :: Text,
    unvalidatedAddressLine2 :: Text,
    unvalidatedAddressLine3 :: Text,
    unvalidatedAddressLine4 :: Text,
    unvalidatedCity :: Text,
    unvalidatedZipCode :: Text
  }
  deriving (Show, Eq)

data UnvalidatedOrderLine = UnvalidatedOrderLine
  { unvalidatedOrderLineId :: Text,
    unvalidatedProductCode :: Text,
    unvalidatedQuantity :: Double
  }
  deriving (Show, Eq)

data UnvalidatedOrder = UnvalidatedOrder
  { unvalidatedOrderId :: Text,
    unvalidatedCustomerInfo :: UnvalidatedCustomerInfo,
    unvalidatedShippingAddress :: UnvalidatedAddress,
    unvalidatedBillingAddress :: UnvalidatedAddress,
    unvalidatedOrderLines :: [UnvalidatedOrderLine]
  }
  deriving (Show, Eq)

data ValidatedOrderLine = ValidatedOrderLine
  { orderLineId :: OrderLineId.OrderLineId,
    productCode :: ProductCode.ProductCode,
    quantity :: OrderQuantity.OrderQuantity
  }
  deriving (Show, Eq)

data ValidatedOrder = ValidatedOrder
  { orderId :: OrderId.OrderId,
    customerInfo :: CustomerInfo.CustomerInfo,
    shippingAddress :: Address.Address,
    billingAddress :: Address.Address,
    orderLines :: [ValidatedOrderLine]
  }
  deriving (Show, Eq)

type CheckProductCodeExists = ProductCode.ProductCode -> Bool

type CheckedAddress = Address.Address

type CheckAddressExists = Address.Address -> EitherIO DomainError CheckedAddress

validateOrder ::
  CheckProductCodeExists ->
  CheckAddressExists ->
  UnvalidatedOrder ->
  EitherIO DomainError ValidatedOrder
validateOrder checkProductCodeExists checkAddressExists unvalidatedOrder = do
  -- validate order ID
  orderId <-
    liftEither $
      OrderId.create $
        unvalidatedOrderId unvalidatedOrder

  -- validate customer info
  customerInfo <-
    liftEither $
      validateCustomerInfo $
        unvalidatedCustomerInfo unvalidatedOrder

  -- validate order lines
  orderLines <-
    fromList $
      liftEither . validateOrderLine checkProductCodeExists
        <$> unvalidatedOrderLines unvalidatedOrder

  -- validate shipping address
  shippingAddress <-
    validateAddress checkAddressExists (unvalidatedShippingAddress unvalidatedOrder)

  -- validate billing address
  billingAddress <-
    validateAddress checkAddressExists (unvalidatedBillingAddress unvalidatedOrder)

  -- construct validate order
  return
    ValidatedOrder
      { orderId = orderId,
        customerInfo = customerInfo,
        shippingAddress = shippingAddress,
        billingAddress = billingAddress,
        orderLines = orderLines
      }

-- private functions

validateCustomerInfo ::
  UnvalidatedCustomerInfo ->
  Either DomainError CustomerInfo.CustomerInfo
validateCustomerInfo input = do
  -- validate name
  let nameParams =
        PersonalName.Params
          { PersonalName.firstName = unvalidatedFirstName input,
            PersonalName.lastName = unvalidatedLastName input
          }
  validatedName <- PersonalName.create nameParams

  -- validate email address
  validatedEmailAddress <- EmailAddress.create $ unvalidatedEmailAddress input
  return
    ( CustomerInfo.CustomerInfo
        { CustomerInfo.personalName = validatedName,
          CustomerInfo.emailAddress = validatedEmailAddress
        }
    )

validateAddress ::
  CheckAddressExists ->
  UnvalidatedAddress ->
  EitherIO DomainError Address.Address
validateAddress checkAddressExists input = do
  -- validate address format
  let params =
        Address.Params
          { Address.addressLine1 = unvalidatedAddressLine1 input,
            Address.addressLine2 = unvalidatedAddressLine2 input,
            Address.addressLine3 = unvalidatedAddressLine3 input,
            Address.addressLine4 = unvalidatedAddressLine4 input,
            Address.city = unvalidatedCity input,
            Address.zipCode = unvalidatedZipCode input
          }
  let formatCheckedAddress = Address.create params

  -- check if address exists
  case formatCheckedAddress of
    (Left err) -> liftEither $ Left err
    (Right address) -> do
      checkAddressExists address

validateOrderLine ::
  CheckProductCodeExists ->
  UnvalidatedOrderLine ->
  Either DomainError ValidatedOrderLine
validateOrderLine checkProductCodeExists input = do
  -- validate order line ID
  orderLineId <-
    OrderLineId.create $
      unvalidatedOrderLineId input

  -- validate product code
  productCode <-
    validateProductCode checkProductCodeExists (unvalidatedProductCode input)

  -- validate quantity
  quantity <-
    OrderQuantity.create productCode (unvalidatedQuantity input)

  return $
    ValidatedOrderLine
      { orderLineId = orderLineId,
        productCode = productCode,
        quantity = quantity
      }

validateProductCode ::
  CheckProductCodeExists ->
  Text ->
  Either DomainError ProductCode.ProductCode
validateProductCode checkProductCodeExists input = do
  -- validate product code format
  productCode <-
    ProductCode.create input
  let codeExists = checkProductCodeExists productCode

  -- check if product code exists
  if codeExists
    then return productCode
    else Left ("product code " <> ProductCode.value productCode <> " does not exist")
