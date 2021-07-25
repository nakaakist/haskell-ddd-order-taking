{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Workflows.PlaceOrder.CreateEvents where

import           Control.Lens                   ( (^.) )
import           Data.Generics.Labels           ( )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified OrderTaking.Types.BillingAmount
                                               as BillingAmount
import qualified OrderTaking.Types.CustomerInfo.EmailAddress
                                               as EmailAddress
import qualified OrderTaking.Workflows.PlaceOrder.Events
                                               as Events
import qualified OrderTaking.Workflows.PlaceOrder.PriceOrder
                                               as PriceOrder


createEvents
  :: PriceOrder.PricedOrder
  -> Maybe Events.OrderAcknowledgmentSent
  -> Events.PlaceOrderEvents
createEvents input orderAcknowledgementSent = Events.PlaceOrderEvents
  { orderPlaced              = createOrderPlacedEvent input
  , billableOrderPlaced      = createBillingEvent input
  , orderAcknowledgementSent
  }


-- private functions

createOrderPlacedEvent :: PriceOrder.PricedOrder -> Events.OrderPlaced
createOrderPlacedEvent input = input

createBillingEvent
  :: PriceOrder.PricedOrder -> Maybe Events.BillableOrderPlaced
createBillingEvent input =
  let shouldBeBilled = BillingAmount.shouldBeBilled $ input ^. #amountToBill
  in  if shouldBeBilled
        then Just $ Events.BillableOrderPlaced
          { orderId        = input ^. #orderId
          , billingAddress = input ^. #billingAddress
          , amountToBill   = input ^. #amountToBill
          }
        else Nothing
