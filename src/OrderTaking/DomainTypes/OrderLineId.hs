{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.DomainTypes.OrderLineId
  ( OrderLineId
  , create
  , value
  ) where

import           Data.Text                      ( Text )
import           OrderTaking.Shared.DomainError ( DomainError )
import           OrderTaking.Shared.UtilFunctions
                                                ( createStringInLengthRange )


newtype OrderLineId = OrderLineIdPrivate Text deriving (Show, Eq)

create :: Text -> Either DomainError OrderLineId
create i =
  OrderLineIdPrivate <$> createStringInLengthRange i "order line ID" 0 50

value :: OrderLineId -> Text
value (OrderLineIdPrivate i) = i
