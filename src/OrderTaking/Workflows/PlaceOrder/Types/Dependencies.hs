{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Workflows.PlaceOrder.Types.Dependencies
  ( CheckProductCodeExists
  , CheckAddressExists
  , GetProductPrice
  , CreateOrderAcknowledgmentLetter
  , SendOrderAcknowledgment
  , HtmlString
  , OrderAcknowledgment(..)
  , SendResult(..)
  ) where

import           Data.Generics.Labels           ( )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import           OrderTaking.Shared.DomainError ( DomainError )
import           OrderTaking.Shared.EitherIO    ( EitherIO )
import qualified OrderTaking.Types.Address.Address
                                               as Address
import qualified OrderTaking.Types.CustomerInfo.EmailAddress
                                               as EmailAddress
import qualified OrderTaking.Types.Price       as Price
import qualified OrderTaking.Types.ProductCode.ProductCode
                                               as ProductCode
import qualified OrderTaking.Workflows.PlaceOrder.Types.Outputs
                                               as Outputs


-- public dependency types

type CheckProductCodeExists = ProductCode.ProductCode -> Bool

type CheckedAddress = Address.Address

type CheckAddressExists
  = Address.Address -> EitherIO DomainError CheckedAddress

type GetProductPrice
  = ProductCode.ProductCode -> Either DomainError Price.Price

type CreateOrderAcknowledgmentLetter = Outputs.PricedOrder -> HtmlString

type SendOrderAcknowledgment = OrderAcknowledgment -> IO SendResult

-- helper types for acknowledgement dependencies

newtype HtmlString = HtmlString Text deriving (Show, Eq, Generic)

data OrderAcknowledgment = OrderAcknowledgment
  { emailAddress :: EmailAddress.EmailAddress
  , letter       :: HtmlString
  }
  deriving (Show, Eq, Generic)

data SendResult = Sent | NotSent
