{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Workflows.PlaceOrder.Subflows.AcknowledgeOrder where


import           Control.Lens                   ( (^.) )
import           Data.Generics.Labels           ( )
import qualified OrderTaking.Workflows.PlaceOrder.DomainTypes.Dependencies
                                               as Dependencies
import qualified OrderTaking.Workflows.PlaceOrder.DomainTypes.Outputs
                                               as Outputs

-- public workflow function

acknowledgeOrder
  :: Dependencies.CreateOrderAcknowledgmentLetter
  -> Dependencies.SendOrderAcknowledgment
  -> Outputs.PricedOrder
  -> IO (Maybe Outputs.OrderAcknowledgementSent)
acknowledgeOrder createOrderAcknowledgmentLetter sendOrderAcknowledgment input
  = do
    -- create acknowledge letter
    let letter = createOrderAcknowledgmentLetter input
    let acknowledgment = Dependencies.OrderAcknowledgment
          { emailAddress = input ^. #customerInfo . #emailAddress
          , letter
          }

    -- send letter
    sendResult <- sendOrderAcknowledgment acknowledgment
    case sendResult of
      Dependencies.Sent -> return $ Just $ Outputs.OrderAcknowledgementSent
        { orderId      = input ^. #orderId
        , emailAddress = input ^. #customerInfo . #emailAddress
        }
      Dependencies.NotSent -> return Nothing
