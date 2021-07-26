{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Workflows.PlaceOrder.Subflows.ValidateOrder
  ( ValidatedOrder(..)
  , ValidatedOrderLine(..)
  , validateOrder
  ) where

import           Control.Lens                   ( (^.) )
import           Data.Generics.Labels           ( )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified OrderTaking.DomainTypes.Address.Address
                                               as Address
import qualified OrderTaking.DomainTypes.CustomerInfo.CustomerInfo
                                               as CustomerInfo
import qualified OrderTaking.DomainTypes.CustomerInfo.EmailAddress
                                               as EmailAddress
import qualified OrderTaking.DomainTypes.CustomerInfo.PersonalName
                                               as PersonalName
import qualified OrderTaking.DomainTypes.OrderId
                                               as OrderId
import qualified OrderTaking.DomainTypes.OrderLineId
                                               as OrderLineId
import qualified OrderTaking.DomainTypes.OrderQuantity.OrderQuantity
                                               as OrderQuantity
import qualified OrderTaking.DomainTypes.ProductCode.ProductCode
                                               as ProductCode
import           OrderTaking.Shared.DomainError ( DomainError )
import           OrderTaking.Shared.EitherIO    ( EitherIO
                                                , fromList
                                                , liftEither
                                                )
import qualified OrderTaking.Workflows.PlaceOrder.DomainTypes.Dependencies
                                               as Dependencies
import qualified OrderTaking.Workflows.PlaceOrder.DomainTypes.Inputs
                                               as Inputs


-- public types

data ValidatedOrderLine = ValidatedOrderLine
  { orderLineId :: OrderLineId.OrderLineId
  , productCode :: ProductCode.ProductCode
  , quantity    :: OrderQuantity.OrderQuantity
  }
  deriving (Show, Eq, Generic)

data ValidatedOrder = ValidatedOrder
  { orderId         :: OrderId.OrderId
  , customerInfo    :: CustomerInfo.CustomerInfo
  , shippingAddress :: Address.Address
  , billingAddress  :: Address.Address
  , orderLines      :: [ValidatedOrderLine]
  }
  deriving (Show, Eq, Generic)

-- public workflow function

validateOrder
  :: Dependencies.CheckProductCodeExists
  -> Dependencies.CheckAddressExists
  -> Inputs.UnvalidatedOrder
  -> EitherIO DomainError ValidatedOrder
validateOrder checkProductCodeExists checkAddressExists input = do
  -- validate order ID
  orderId      <- liftEither $ OrderId.create $ input ^. #orderId

  -- validate customer info
  customerInfo <- liftEither $ validateCustomerInfo $ input ^. #customerInfo

  -- validate order lines
  orderLines   <-
    fromList
    $   liftEither
    .   validateOrderLine checkProductCodeExists
    <$> (input ^. #orderLines)

  -- validate shipping address
  shippingAddress <- validateAddress checkAddressExists
                                     (input ^. #shippingAddress)

  -- validate billing address
  billingAddress <- validateAddress checkAddressExists
                                    (input ^. #billingAddress)

  -- construct validate order
  return ValidatedOrder { orderId
                        , customerInfo
                        , shippingAddress
                        , billingAddress
                        , orderLines
                        }


-- private functions

validateCustomerInfo
  :: Inputs.UnvalidatedCustomerInfo
  -> Either DomainError CustomerInfo.CustomerInfo
validateCustomerInfo input = do
  -- validate name
  let nameParams = PersonalName.Params { firstName = input ^. #firstName
                                       , lastName  = input ^. #lastName
                                       }
  validatedName         <- PersonalName.create nameParams

  -- validate email address
  validatedEmailAddress <- EmailAddress.create (input ^. #emailAddress)
  return
    (CustomerInfo.CustomerInfo { personalName = validatedName
                               , emailAddress = validatedEmailAddress
                               }
    )

validateAddress
  :: Dependencies.CheckAddressExists
  -> Inputs.UnvalidatedAddress
  -> EitherIO DomainError Address.Address
validateAddress checkAddressExists input = do
  -- validate address format
  let params = Address.Params { addressLine1 = input ^. #addressLine1
                              , addressLine2 = input ^. #addressLine2
                              , addressLine3 = input ^. #addressLine3
                              , addressLine4 = input ^. #addressLine4
                              , city         = input ^. #city
                              , zipCode      = input ^. #zipCode
                              }
  let formatCheckedAddress = Address.create params

  -- check if address exists
  case formatCheckedAddress of
    (Left  err    ) -> liftEither $ Left err
    (Right address) -> do
      checkAddressExists address

validateOrderLine
  :: Dependencies.CheckProductCodeExists
  -> Inputs.UnvalidatedOrderLine
  -> Either DomainError ValidatedOrderLine
validateOrderLine checkProductCodeExists input = do
  -- validate order line ID
  orderLineId <- OrderLineId.create $ input ^. #orderLineId

  -- validate product code
  productCode <- validateProductCode checkProductCodeExists
                                     (input ^. #productCode)

  -- validate quantity
  quantity <- OrderQuantity.create productCode (input ^. #quantity)

  return $ ValidatedOrderLine { orderLineId, productCode, quantity }

validateProductCode
  :: Dependencies.CheckProductCodeExists
  -> Text
  -> Either DomainError ProductCode.ProductCode
validateProductCode checkProductCodeExists input = do
  -- validate product code format
  productCode <- ProductCode.create input
  let codeExists = checkProductCodeExists productCode

  -- check if product code exists
  if codeExists
    then return productCode
    else Left
      ("product code " <> ProductCode.value productCode <> " does not exist")
