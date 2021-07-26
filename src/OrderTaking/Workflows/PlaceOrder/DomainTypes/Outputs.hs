{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Workflows.PlaceOrder.DomainTypes.Outputs
  ( PlaceOrderEvents(..)
  , OrderPlaced
  , PricedOrder(..)
  , PricedOrderLine(..)
  , OrderAcknowledgementSent(..)
  , BillableOrderPlaced(..)
  ) where

import           GHC.Generics                   ( Generic )
import qualified OrderTaking.DomainTypes.Address.Address
                                               as Address
import qualified OrderTaking.DomainTypes.BillingAmount
                                               as BillingAmount
import qualified OrderTaking.DomainTypes.CustomerInfo.CustomerInfo
                                               as CustomerInfo
import qualified OrderTaking.DomainTypes.CustomerInfo.EmailAddress
                                               as EmailAddress
import qualified OrderTaking.DomainTypes.OrderId
                                               as OrderId
import qualified OrderTaking.DomainTypes.OrderLineId
                                               as OrderLineId
import qualified OrderTaking.DomainTypes.OrderQuantity.OrderQuantity
                                               as OrderQuantity
import qualified OrderTaking.DomainTypes.Price as Price
import qualified OrderTaking.DomainTypes.ProductCode.ProductCode
                                               as ProductCode

-- public output event types

data PlaceOrderEvents = PlaceOrderEvents
  { orderPlaced              :: OrderPlaced
  , billableOrderPlaced      :: Maybe BillableOrderPlaced
  , orderAcknowledgementSent :: Maybe OrderAcknowledgementSent
  }
  deriving (Show, Eq, Generic)

-- order placed event
type OrderPlaced = PricedOrder

data PricedOrder = PricedOrder
  { orderId         :: OrderId.OrderId
  , customerInfo    :: CustomerInfo.CustomerInfo
  , shippingAddress :: Address.Address
  , billingAddress  :: Address.Address
  , amountToBill    :: BillingAmount.BillingAmount
  , orderLines      :: [PricedOrderLine]
  }
  deriving (Show, Eq, Generic)

data PricedOrderLine = PricedOrderLine
  { orderLineId :: OrderLineId.OrderLineId
  , productCode :: ProductCode.ProductCode
  , quantity    :: OrderQuantity.OrderQuantity
  , linePrice   :: Price.Price
  }
  deriving (Show, Eq, Generic)

-- order acknowledgement sent event
data OrderAcknowledgementSent = OrderAcknowledgementSent
  { orderId      :: OrderId.OrderId
  , emailAddress :: EmailAddress.EmailAddress
  }
  deriving (Show, Eq, Generic)

-- billable order placed event
data BillableOrderPlaced = BillableOrderPlaced
  { orderId        :: OrderId.OrderId
  , billingAddress :: Address.Address
  , amountToBill   :: BillingAmount.BillingAmount
  }
  deriving (Show, Eq, Generic)
