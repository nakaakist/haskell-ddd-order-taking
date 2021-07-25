{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Workflows.PlaceOrder.AcknowledgeOrder where


import           Control.Lens                   ( (^.) )
import           Data.Generics.Labels           ( )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified OrderTaking.Types.CustomerInfo.EmailAddress
                                               as EmailAddress
import qualified OrderTaking.Workflows.PlaceOrder.Events
                                               as Events
import qualified OrderTaking.Workflows.PlaceOrder.PriceOrder
                                               as PriceOrder

-- public types

newtype HtmlString = HtmlString Text deriving (Show, Eq, Generic)

data OrderAcknowledgment = OrderAcknowledgment
  { emailAddress :: EmailAddress.EmailAddress
  , letter       :: HtmlString
  }
  deriving (Show, Eq, Generic)

data SendResult = Sent | NotSent

-- dependency types

type CreateOrderAcknowledgmentLetter = PriceOrder.PricedOrder -> HtmlString

type SendOrderAcknowledgment = OrderAcknowledgment -> IO SendResult


-- public workflow function

acknowledgeOrder
  :: CreateOrderAcknowledgmentLetter
  -> SendOrderAcknowledgment
  -> PriceOrder.PricedOrder
  -> IO (Maybe Events.OrderAcknowledgmentSent)
acknowledgeOrder createOrderAcknowledgmentLetter sendOrderAcknowledgment input
  = do
    -- create acknowledge letter
    let letter = createOrderAcknowledgmentLetter input
    let acknowledgment = OrderAcknowledgment
          { emailAddress = input ^. #customerInfo . #emailAddress
          , letter
          }

    -- send letter
    sendResult <- sendOrderAcknowledgment acknowledgment
    case sendResult of
      Sent -> return $ Just $ Events.OrderAcknowledgmentSent
        { orderId      = input ^. #orderId
        , emailAddress = input ^. #customerInfo . #emailAddress
        }
      NotSent -> return Nothing
