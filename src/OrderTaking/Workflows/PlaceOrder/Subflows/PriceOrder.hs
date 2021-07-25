{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Workflows.PlaceOrder.Subflows.PriceOrder
  ( priceOrder
  ) where

import           Control.Lens                   ( (^.) )
import           Data.Generics.Labels           ( )
import           OrderTaking.Shared.DomainError ( DomainError )
import           OrderTaking.Shared.EitherIO    ( EitherIO
                                                , fromList
                                                , liftEither
                                                )
import qualified OrderTaking.Types.BillingAmount
                                               as BillingAmount
import qualified OrderTaking.Types.OrderQuantity.OrderQuantity
                                               as OrderQuantity
import qualified OrderTaking.Types.Price       as Price
import qualified OrderTaking.Workflows.PlaceOrder.Subflows.ValidateOrder
                                               as ValidateOrder
import qualified OrderTaking.Workflows.PlaceOrder.Types.Dependencies
                                               as Dependencies
import qualified OrderTaking.Workflows.PlaceOrder.Types.Outputs
                                               as Outputs


-- public workflow function

priceOrder
  :: Dependencies.GetProductPrice
  -> ValidateOrder.ValidatedOrder
  -> EitherIO DomainError Outputs.PricedOrder
priceOrder getProductPrice input = do
  -- price order lines
  let validatedLines = input ^. #orderLines
  pricedLines <-
    fromList $ liftEither . priceOrderLine getProductPrice <$> validatedLines

  -- calculate total billing amount
  amountToBill <-
    liftEither $ BillingAmount.sumPrices $ (^. #linePrice) <$> pricedLines

  -- construct priced order
  return $ Outputs.PricedOrder { orderId         = input ^. #orderId
                               , customerInfo    = input ^. #customerInfo
                               , shippingAddress = input ^. #shippingAddress
                               , billingAddress  = input ^. #billingAddress
                               , amountToBill
                               , orderLines      = pricedLines
                               }


-- private functions

priceOrderLine
  :: Dependencies.GetProductPrice
  -> ValidateOrder.ValidatedOrderLine
  -> Either DomainError Outputs.PricedOrderLine
priceOrderLine getProductPrice input = do
  let productCode = input ^. #productCode
  let quantity    = input ^. #quantity

  -- get price from catalog
  price     <- getProductPrice productCode

  -- calculate line price from price and quantity
  linePrice <- Price.multiply (OrderQuantity.value quantity) price

  -- construct priced order line
  return $ Outputs.PricedOrderLine { orderLineId = input ^. #orderLineId
                                   , productCode
                                   , quantity
                                   , linePrice
                                   }
