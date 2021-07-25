{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Workflows.PlaceOrder.ValidateOrder
  ( UnvalidatedCustomerInfo (..),
    UnvalidatedAddress (..),
    UnvalidatedOrderLine (..),
    UnvalidatedOrder (..),
    ValidatedOrder (..),
    ValidatedOrderLine (..),
    CheckProductCodeExists,
    CheckAddressExists,
    validateOrder,
  )
where

import Control.Lens ((^.))
import Data.Generics.Labels ()
import Data.Text (Text)
import GHC.Generics (Generic)
import OrderTaking.Shared.DomainError (DomainError)
import OrderTaking.Shared.EitherIO (EitherIO, fromList, liftEither, liftIO, runEitherIO)
import qualified OrderTaking.Types.Address.Address as Address
import qualified OrderTaking.Types.CustomerInfo.CustomerInfo as CustomerInfo
import qualified OrderTaking.Types.CustomerInfo.EmailAddress as EmailAddress
import qualified OrderTaking.Types.CustomerInfo.PersonalName as PersonalName
import qualified OrderTaking.Types.OrderId as OrderId
import qualified OrderTaking.Types.OrderLineId as OrderLineId
import qualified OrderTaking.Types.OrderQuantity.OrderQuantity as OrderQuantity
import qualified OrderTaking.Types.ProductCode.ProductCode as ProductCode

data UnvalidatedCustomerInfo = UnvalidatedCustomerInfo
  { firstName :: Text,
    lastName :: Text,
    emailAddress :: Text
  }
  deriving (Show, Eq, Generic)

data UnvalidatedAddress = UnvalidatedAddress
  { addressLine1 :: Text,
    addressLine2 :: Text,
    addressLine3 :: Text,
    addressLine4 :: Text,
    city :: Text,
    zipCode :: Text
  }
  deriving (Show, Eq, Generic)

data UnvalidatedOrderLine = UnvalidatedOrderLine
  { orderLineId :: Text,
    productCode :: Text,
    quantity :: Double
  }
  deriving (Show, Eq, Generic)

data UnvalidatedOrder = UnvalidatedOrder
  { orderId :: Text,
    customerInfo :: UnvalidatedCustomerInfo,
    shippingAddress :: UnvalidatedAddress,
    billingAddress :: UnvalidatedAddress,
    orderLines :: [UnvalidatedOrderLine]
  }
  deriving (Show, Eq, Generic)

data ValidatedOrderLine = ValidatedOrderLine
  { orderLineId :: OrderLineId.OrderLineId,
    productCode :: ProductCode.ProductCode,
    quantity :: OrderQuantity.OrderQuantity
  }
  deriving (Show, Eq, Generic)

data ValidatedOrder = ValidatedOrder
  { orderId :: OrderId.OrderId,
    customerInfo :: CustomerInfo.CustomerInfo,
    shippingAddress :: Address.Address,
    billingAddress :: Address.Address,
    orderLines :: [ValidatedOrderLine]
  }
  deriving (Show, Eq, Generic)

type CheckProductCodeExists = ProductCode.ProductCode -> Bool

type CheckedAddress = Address.Address

type CheckAddressExists = Address.Address -> EitherIO DomainError CheckedAddress

validateOrder ::
  CheckProductCodeExists ->
  CheckAddressExists ->
  UnvalidatedOrder ->
  EitherIO DomainError ValidatedOrder
validateOrder checkProductCodeExists checkAddressExists input = do
  -- validate order ID
  orderId <-
    liftEither $
      OrderId.create $
        input ^. #orderId

  -- validate customer info
  customerInfo <-
    liftEither $
      validateCustomerInfo $
        input ^. #customerInfo

  -- validate order lines
  orderLines <-
    fromList $
      liftEither . validateOrderLine checkProductCodeExists
        <$> (input ^. #orderLines)

  -- validate shipping address
  shippingAddress <-
    validateAddress checkAddressExists (input ^. #shippingAddress)

  -- validate billing address
  billingAddress <-
    validateAddress checkAddressExists (input ^. #billingAddress)

  -- construct validate order
  return
    ValidatedOrder
      { orderId,
        customerInfo,
        shippingAddress,
        billingAddress,
        orderLines
      }

-- private functions

validateCustomerInfo ::
  UnvalidatedCustomerInfo ->
  Either DomainError CustomerInfo.CustomerInfo
validateCustomerInfo input = do
  -- validate name
  let nameParams =
        PersonalName.Params
          { PersonalName.firstName = input ^. #firstName,
            PersonalName.lastName = input ^. #lastName
          }
  validatedName <- PersonalName.create nameParams

  -- validate email address
  validatedEmailAddress <-
    EmailAddress.create
      (input ^. #emailAddress)
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
          { Address.addressLine1 = input ^. #addressLine1,
            Address.addressLine2 = input ^. #addressLine2,
            Address.addressLine3 = input ^. #addressLine3,
            Address.addressLine4 = input ^. #addressLine4,
            Address.city = input ^. #city,
            Address.zipCode = input ^. #zipCode
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
      input ^. #orderLineId

  -- validate product code
  productCode <-
    validateProductCode checkProductCodeExists (input ^. #productCode)

  -- validate quantity
  quantity <-
    OrderQuantity.create productCode (input ^. #quantity)

  return $
    ValidatedOrderLine
      { orderLineId,
        productCode,
        quantity
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
