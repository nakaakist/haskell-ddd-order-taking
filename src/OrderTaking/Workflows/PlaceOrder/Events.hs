{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Workflows.PlaceOrder.Events
  ( OrderAcknowledgmentSent(..)
  , OrderPlaced(..)
  , BillableOrderPlaced(..)
  ) where

import           GHC.Generics                   ( Generic )
import qualified OrderTaking.Types.Address.Address
                                               as Address
import qualified OrderTaking.Types.BillingAmount
                                               as BillingAmount
import qualified OrderTaking.Types.CustomerInfo.EmailAddress
                                               as EmailAddress
import qualified OrderTaking.Types.OrderId     as OrderId
import qualified OrderTaking.Workflows.PlaceOrder.PriceOrder
                                               as PriceOrder


data OrderAcknowledgmentSent = OrderAcknowledgmentSent
  { orderId      :: OrderId.OrderId
  , emailAddress :: EmailAddress.EmailAddress
  }
  deriving (Show, Eq, Generic)

type OrderPlaced = PriceOrder.PricedOrder

data BillableOrderPlaced = BillableOrderPlaced
  { orderId        :: OrderId.OrderId
  , billingAddress :: Address.Address
  , amountToBill   :: BillingAmount.BillingAmount
  }
  deriving (Show, Eq, Generic)
