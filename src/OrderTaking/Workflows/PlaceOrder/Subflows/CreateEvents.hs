{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Workflows.PlaceOrder.Subflows.CreateEvents
  ( createEvents
  ) where

import           Control.Lens                   ( (^.) )
import           Data.Generics.Labels           ( )
import qualified OrderTaking.DomainTypes.BillingAmount
                                               as BillingAmount
import qualified OrderTaking.Workflows.PlaceOrder.DomainTypes.Outputs
                                               as Outputs


-- public workflow function

createEvents
  :: Outputs.PricedOrder
  -> Maybe Outputs.OrderAcknowledgementSent
  -> Outputs.PlaceOrderEvents
createEvents input orderAcknowledgementSent = Outputs.PlaceOrderEvents
  { orderPlaced              = createOrderPlacedEvent input
  , billableOrderPlaced      = createBillingEvent input
  , orderAcknowledgementSent
  }


-- private functions

createOrderPlacedEvent :: Outputs.PricedOrder -> Outputs.OrderPlaced
createOrderPlacedEvent input = input

createBillingEvent :: Outputs.PricedOrder -> Maybe Outputs.BillableOrderPlaced
createBillingEvent input =
  let shouldBeBilled = BillingAmount.shouldBeBilled $ input ^. #amountToBill
  in  if shouldBeBilled
        then Just $ Outputs.BillableOrderPlaced
          { orderId        = input ^. #orderId
          , billingAddress = input ^. #billingAddress
          , amountToBill   = input ^. #amountToBill
          }
        else Nothing
