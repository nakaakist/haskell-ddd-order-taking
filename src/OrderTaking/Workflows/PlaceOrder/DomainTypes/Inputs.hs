{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Workflows.PlaceOrder.DomainTypes.Inputs
  ( UnvalidatedCustomerInfo(..)
  , UnvalidatedAddress(..)
  , UnvalidatedOrderLine(..)
  , UnvalidatedOrder(..)
  ) where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Generics.Labels           ( )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )


-- public input types

data UnvalidatedOrder = UnvalidatedOrder
  { orderId         :: Text
  , customerInfo    :: UnvalidatedCustomerInfo
  , shippingAddress :: UnvalidatedAddress
  , billingAddress  :: UnvalidatedAddress
  , orderLines      :: [UnvalidatedOrderLine]
  }
  deriving (Show, Eq, Generic)
instance ToJSON UnvalidatedOrder
instance FromJSON UnvalidatedOrder

data UnvalidatedCustomerInfo = UnvalidatedCustomerInfo
  { firstName    :: Text
  , lastName     :: Text
  , emailAddress :: Text
  }
  deriving (Show, Eq, Generic)
instance ToJSON UnvalidatedCustomerInfo
instance FromJSON UnvalidatedCustomerInfo

data UnvalidatedAddress = UnvalidatedAddress
  { addressLine1 :: Text
  , addressLine2 :: Text
  , addressLine3 :: Text
  , addressLine4 :: Text
  , city         :: Text
  , zipCode      :: Text
  }
  deriving (Show, Eq, Generic)
instance ToJSON UnvalidatedAddress
instance FromJSON UnvalidatedAddress

data UnvalidatedOrderLine = UnvalidatedOrderLine
  { orderLineId :: Text
  , productCode :: Text
  , quantity    :: Double
  }
  deriving (Show, Eq, Generic)
instance ToJSON UnvalidatedOrderLine
instance FromJSON UnvalidatedOrderLine
