{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Workflows.PlaceOrder.Types.Inputs
  ( UnvalidatedCustomerInfo(..)
  , UnvalidatedAddress(..)
  , UnvalidatedOrderLine(..)
  , UnvalidatedOrder(..)
  ) where

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

data UnvalidatedCustomerInfo = UnvalidatedCustomerInfo
  { firstName    :: Text
  , lastName     :: Text
  , emailAddress :: Text
  }
  deriving (Show, Eq, Generic)

data UnvalidatedAddress = UnvalidatedAddress
  { addressLine1 :: Text
  , addressLine2 :: Text
  , addressLine3 :: Text
  , addressLine4 :: Text
  , city         :: Text
  , zipCode      :: Text
  }
  deriving (Show, Eq, Generic)

data UnvalidatedOrderLine = UnvalidatedOrderLine
  { orderLineId :: Text
  , productCode :: Text
  , quantity    :: Double
  }
  deriving (Show, Eq, Generic)
