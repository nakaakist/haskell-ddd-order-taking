{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Workflows.PlaceOrder.Dtos.OutputDtos
  ( PlaceOrderEventsDto(..)
  , OrderPlacedDto
  , PricedOrderDto(..)
  , PricedOrderLineDto(..)
  , OrderAcknowledgementSentDto(..)
  , BillableOrderPlacedDto(..)
  , placeOrderEventsDtoFromDomain
  , pricedOrderDtoFromDomain
  , pricedOrderLineDtoFromDomain
  , orderAcknowledgementSentFromDomain
  , billableOrderPlacedFromDomain
  ) where

import           Control.Lens                   ( (^.) )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Function                  ( (&) )
import           Data.Generics.Labels           ( )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified OrderTaking.DomainTypes.Address.Address
                                               as Address
import qualified OrderTaking.DomainTypes.BillingAmount
                                               as BillingAmount
import qualified OrderTaking.DomainTypes.CustomerInfo.CustomerInfo
                                               as CustomerInfo
import qualified OrderTaking.DomainTypes.CustomerInfo.EmailAddress
                                               as EmailAddress
import qualified OrderTaking.DomainTypes.CustomerInfo.PersonalName
                                               as PersonalName
import qualified OrderTaking.DomainTypes.OrderId
                                               as OrderId
import qualified OrderTaking.DomainTypes.OrderLineId
                                               as OrderLineId
import qualified OrderTaking.DomainTypes.OrderQuantity.OrderQuantity
                                               as OrderQuantity
import qualified OrderTaking.DomainTypes.Price as Price
import qualified OrderTaking.DomainTypes.ProductCode.ProductCode
                                               as ProductCode
import           OrderTaking.Workflows.PlaceOrder.DomainTypes.Outputs
                                               as Outputs


-- public output DTO types

data PlaceOrderEventsDto = PlaceOrderEventsDto
  { orderPlaced              :: OrderPlacedDto
  , billableOrderPlaced      :: Maybe BillableOrderPlacedDto
  , orderAcknowledgementSent :: Maybe OrderAcknowledgementSentDto
  }
  deriving (Show, Eq, Generic)
instance ToJSON PlaceOrderEventsDto
instance FromJSON PlaceOrderEventsDto

-- order placed event
type OrderPlacedDto = PricedOrderDto

data PricedOrderDto = PricedOrderDto
  { orderId         :: Text
  , customerInfo    :: CustomerInfoDto
  , shippingAddress :: AddressDto
  , billingAddress  :: AddressDto
  , amountToBill    :: Double
  , orderLines      :: [PricedOrderLineDto]
  }
  deriving (Show, Eq, Generic)
instance ToJSON PricedOrderDto
instance FromJSON PricedOrderDto

data CustomerInfoDto = CustomerInfoDto
  { firstName    :: Text
  , lastName     :: Text
  , emailAddress :: Text
  }
  deriving (Show, Eq, Generic)
instance ToJSON CustomerInfoDto
instance FromJSON CustomerInfoDto

type AddressDto = Address.Params

data PricedOrderLineDto = PricedOrderLineDto
  { orderLineId :: Text
  , productCode :: Text
  , quantity    :: Double
  , linePrice   :: Double
  }
  deriving (Show, Eq, Generic)
instance ToJSON PricedOrderLineDto
instance FromJSON PricedOrderLineDto

-- order acknowledgement sent event
data OrderAcknowledgementSentDto = OrderAcknowledgementSentDto
  { orderId      :: Text
  , emailAddress :: Text
  }
  deriving (Show, Eq, Generic)
instance ToJSON OrderAcknowledgementSentDto
instance FromJSON OrderAcknowledgementSentDto

-- billable order placed event
data BillableOrderPlacedDto = BillableOrderPlacedDto
  { orderId        :: Text
  , billingAddress :: AddressDto
  , amountToBill   :: Double
  }
  deriving (Show, Eq, Generic)
instance ToJSON BillableOrderPlacedDto
instance FromJSON BillableOrderPlacedDto


-- conversion functions from domain

placeOrderEventsDtoFromDomain
  :: Outputs.PlaceOrderEvents -> PlaceOrderEventsDto
placeOrderEventsDtoFromDomain domainObj =
  let
    orderPlaced = pricedOrderDtoFromDomain $ domainObj ^. #orderPlaced
    billableOrderPlaced =
      billableOrderPlacedFromDomain <$> domainObj ^. #billableOrderPlaced
    orderAcknowledgementSent =
      orderAcknowledgementSentFromDomain
        <$> domainObj
        ^.  #orderAcknowledgementSent
  in
    PlaceOrderEventsDto { orderPlaced
                        , billableOrderPlaced
                        , orderAcknowledgementSent
                        }

pricedOrderDtoFromDomain :: Outputs.PricedOrder -> PricedOrderDto
pricedOrderDtoFromDomain domainObj =
  let orderId         = domainObj ^. #orderId & OrderId.value
      customerInfo    = domainObj ^. #customerInfo & customerInfoDtoFromDomain
      shippingAddress = domainObj ^. #shippingAddress & Address.value
      billingAddress  = domainObj ^. #billingAddress & Address.value
      amountToBill    = domainObj ^. #amountToBill & BillingAmount.value
      orderLines = pricedOrderLineDtoFromDomain <$> domainObj ^. #orderLines
  in  PricedOrderDto { orderId
                     , customerInfo
                     , shippingAddress
                     , billingAddress
                     , amountToBill
                     , orderLines
                     }

customerInfoDtoFromDomain :: CustomerInfo.CustomerInfo -> CustomerInfoDto
customerInfoDtoFromDomain domainObj =
  let personalName = domainObj ^. #personalName & PersonalName.value
      firstName    = personalName ^. #firstName
      lastName     = personalName ^. #lastName
      emailAddress = domainObj ^. #emailAddress & EmailAddress.value
  in  CustomerInfoDto { firstName, lastName, emailAddress }

pricedOrderLineDtoFromDomain :: Outputs.PricedOrderLine -> PricedOrderLineDto
pricedOrderLineDtoFromDomain domainObj =
  let orderLineId = domainObj ^. #orderLineId & OrderLineId.value
      productCode = domainObj ^. #productCode & ProductCode.value
      quantity    = domainObj ^. #quantity & OrderQuantity.value
      linePrice   = domainObj ^. #linePrice & Price.value
  in  PricedOrderLineDto { orderLineId, productCode, quantity, linePrice }

billableOrderPlacedFromDomain
  :: Outputs.BillableOrderPlaced -> BillableOrderPlacedDto
billableOrderPlacedFromDomain domainObj =
  let orderId        = domainObj ^. #orderId & OrderId.value
      billingAddress = domainObj ^. #billingAddress & Address.value
      amountToBill   = domainObj ^. #amountToBill & BillingAmount.value
  in  BillableOrderPlacedDto { orderId, billingAddress, amountToBill }

orderAcknowledgementSentFromDomain
  :: Outputs.OrderAcknowledgementSent -> OrderAcknowledgementSentDto
orderAcknowledgementSentFromDomain domainObj =
  let orderId      = domainObj ^. #orderId & OrderId.value
      emailAddress = domainObj ^. #emailAddress & EmailAddress.value
  in  OrderAcknowledgementSentDto { orderId, emailAddress }
