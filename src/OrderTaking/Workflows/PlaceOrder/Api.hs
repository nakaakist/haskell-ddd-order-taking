{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module OrderTaking.Workflows.PlaceOrder.Api
  ( routes
  ) where

import           Control.Monad.Trans            ( liftIO )
import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified OrderTaking.DomainTypes.Price as Price
import           OrderTaking.Shared.EitherIO    ( runEitherIO )
import qualified OrderTaking.Workflows.PlaceOrder.DomainTypes.Dependencies
                                               as Dependencies
import qualified OrderTaking.Workflows.PlaceOrder.Dtos.InputDtos
                                               as InputDtos
import qualified OrderTaking.Workflows.PlaceOrder.Dtos.OutputDtos
                                               as OutputDtos
import qualified OrderTaking.Workflows.PlaceOrder.PlaceOrder
                                               as PlaceOrder
import           Web.Scotty                     ( ActionM
                                                , ScottyM
                                                , json
                                                , jsonData
                                                , post
                                                )


routes :: ScottyM ()
routes = do
  post "/orders" $ do
    unvalidatedOrder <- jsonData :: ActionM InputDtos.UnvalidatedOrderDto
    result           <- liftIO $ runEitherIO $ PlaceOrder.placeOrder
      mockCheckProductCode
      mockCheckAddress
      mockGetProductPrice
      mockCreateOrderAcknowledgementLetter
      mockSendOrderAcknowledgment
      unvalidatedOrder
    case result of
      Left err -> json $ ErrorResponse { message = err }
      Right placeOrderEvents ->
        json $ OutputDtos.placeOrderEventsDtoFromDomain placeOrderEvents


-- internal types and functions

-- error response

newtype ErrorResponse = ErrorResponse {message:: Text} deriving (Show, Generic)
instance ToJSON ErrorResponse
instance FromJSON ErrorResponse

-- dependencies (mock)

mockCheckProductCode :: Dependencies.CheckProductCodeExists
mockCheckProductCode _ = True

mockCheckAddress :: Dependencies.CheckAddressExists
mockCheckAddress = return

mockGetProductPrice :: Dependencies.GetProductPrice
mockGetProductPrice _ = Price.create 10.0

mockCreateOrderAcknowledgementLetter
  :: Dependencies.CreateOrderAcknowledgmentLetter
mockCreateOrderAcknowledgementLetter _ = Dependencies.HtmlString "mock letter"

mockSendOrderAcknowledgment :: Dependencies.SendOrderAcknowledgment
mockSendOrderAcknowledgment _ = return Dependencies.Sent
