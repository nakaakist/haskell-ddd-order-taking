{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Workflows.PlaceOrder.Types.Outputs
  ( PlaceOrderEvents(..)
  , OrderPlaced
  , PricedOrder(..)
  , PricedOrderLine(..)
  , OrderAcknowledgementSent(..)
  , BillableOrderPlaced(..)
  ) where

import           GHC.Generics                   ( Generic )
import qualified OrderTaking.Types.Address.Address
                                               as Address
import qualified OrderTaking.Types.BillingAmount
                                               as BillingAmount
import qualified OrderTaking.Types.CustomerInfo.CustomerInfo
                                               as CustomerInfo
import qualified OrderTaking.Types.CustomerInfo.EmailAddress
                                               as EmailAddress
import qualified OrderTaking.Types.OrderId     as OrderId
import qualified OrderTaking.Types.OrderLineId as OrderLineId
import qualified OrderTaking.Types.OrderQuantity.OrderQuantity
                                               as OrderQuantity
import qualified OrderTaking.Types.Price       as Price
import qualified OrderTaking.Types.ProductCode.ProductCode
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
