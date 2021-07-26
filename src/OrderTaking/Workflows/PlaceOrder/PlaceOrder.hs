{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Workflows.PlaceOrder.PlaceOrder
  ( placeOrder
  ) where

import           Data.Generics.Labels           ( )
import           OrderTaking.Shared.DomainError ( DomainError )
import           OrderTaking.Shared.EitherIO    ( EitherIO
                                                , liftIO
                                                )
import qualified OrderTaking.Workflows.PlaceOrder.DomainTypes.Dependencies
                                               as Dependencies
import qualified OrderTaking.Workflows.PlaceOrder.DomainTypes.Inputs
                                               as Inputs
import qualified OrderTaking.Workflows.PlaceOrder.DomainTypes.Outputs
                                               as Outputs
import qualified OrderTaking.Workflows.PlaceOrder.Subflows.AcknowledgeOrder
                                               as AcknowledgeOrder
import qualified OrderTaking.Workflows.PlaceOrder.Subflows.CreateEvents
                                               as CreateEvents
import qualified OrderTaking.Workflows.PlaceOrder.Subflows.PriceOrder
                                               as PriceOrder
import qualified OrderTaking.Workflows.PlaceOrder.Subflows.ValidateOrder
                                               as ValidateOrder
-- public workflow function

placeOrder
  :: Dependencies.CheckProductCodeExists
  -> Dependencies.CheckAddressExists
  -> Dependencies.GetProductPrice
  -> Dependencies.CreateOrderAcknowledgmentLetter
  -> Dependencies.SendOrderAcknowledgment
  -> Inputs.UnvalidatedOrder
  -> EitherIO DomainError Outputs.PlaceOrderEvents
placeOrder checkProductCodeExists checkAddressExists getProductPrice createOrderAcknowledgmentLetter sendOrderAcknowledgment input
  = do
    -- validate order
    validatedOrder <- ValidateOrder.validateOrder checkProductCodeExists
                                                  checkAddressExists
                                                  input

    -- price order
    pricedOrder         <- PriceOrder.priceOrder getProductPrice validatedOrder

    -- acknowledge order
    acknowledgementSent <- liftIO $ AcknowledgeOrder.acknowledgeOrder
      createOrderAcknowledgmentLetter
      sendOrderAcknowledgment
      pricedOrder

    -- create events
    return $ CreateEvents.createEvents pricedOrder acknowledgementSent
