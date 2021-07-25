{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Workflows.PlaceOrder.PriceOrder
  ( PricedOrderLine (..),
    PricedOrder (..),
    GetProductPrice,
    priceOrder,
  )
where

import Control.Lens ((^.))
import Data.Generics.Labels ()
import Data.Text (Text)
import GHC.Generics (Generic)
import OrderTaking.Shared.DomainError (DomainError)
import OrderTaking.Shared.EitherIO (EitherIO, fromList, liftEither, liftIO, runEitherIO)
import qualified OrderTaking.Types.Address.Address as Address
import qualified OrderTaking.Types.BillingAmount as BillingAmount
import qualified OrderTaking.Types.CustomerInfo.CustomerInfo as CustomerInfo
import qualified OrderTaking.Types.OrderId as OrderId
import qualified OrderTaking.Types.OrderLineId as OrderLineId
import qualified OrderTaking.Types.OrderQuantity.OrderQuantity as OrderQuantity
import qualified OrderTaking.Types.Price as Price
import qualified OrderTaking.Types.ProductCode.ProductCode as ProductCode
import qualified OrderTaking.Workflows.PlaceOrder.ValidateOrder as ValidateOrder

data PricedOrderLine = PricedOrderLine
  { orderLineId :: OrderLineId.OrderLineId,
    productCode :: ProductCode.ProductCode,
    quantity :: OrderQuantity.OrderQuantity,
    linePrice :: Price.Price
  }
  deriving (Show, Eq)

data PricedOrder = PricedOrder
  { orderId :: OrderId.OrderId,
    customerInfo :: CustomerInfo.CustomerInfo,
    shippingAddress :: Address.Address,
    billingAddress :: Address.Address,
    amountToBill :: BillingAmount.BillingAmount,
    orderLines :: [PricedOrderLine]
  }
  deriving (Show, Eq)

type GetProductPrice = ProductCode.ProductCode -> Either DomainError Price.Price

priceOrder ::
  GetProductPrice ->
  ValidateOrder.ValidatedOrder ->
  EitherIO DomainError PricedOrder
priceOrder getProductPrice input = do
  -- price order lines
  let validatedLines = input ^. #orderLines
  pricedLines <-
    fromList $
      liftEither . priceOrderLine getProductPrice <$> validatedLines

  -- calculate total billing amount
  amountToBill <-
    liftEither $
      BillingAmount.sumPrices $ linePrice <$> pricedLines

  -- construct priced order
  return $
    PricedOrder
      { orderId = input ^. #orderId,
        customerInfo = input ^. #customerInfo,
        shippingAddress = input ^. #shippingAddress,
        billingAddress = input ^. #billingAddress,
        amountToBill,
        orderLines = pricedLines
      }

-- private functions

priceOrderLine ::
  GetProductPrice ->
  ValidateOrder.ValidatedOrderLine ->
  Either DomainError PricedOrderLine
priceOrderLine getProductPrice input = do
  let productCode = input ^. #productCode
  let quantity = input ^. #quantity

  -- get price from catalog
  price <- getProductPrice productCode

  -- calculate line price from price and quantity
  linePrice <- Price.multiply (OrderQuantity.value quantity) price

  -- construct priced order line
  return $
    PricedOrderLine
      { orderLineId = input ^. #orderLineId,
        productCode,
        quantity,
        linePrice
      }
