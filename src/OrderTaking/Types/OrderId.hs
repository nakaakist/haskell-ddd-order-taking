{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.OrderId
  ( OrderId,
    create,
    value,
  )
where

import Data.Text (Text)
import OrderTaking.Shared (DomainError, createStringInLengthRange)

newtype OrderId = OrderIdPrivate Text deriving (Show, Eq)

create :: Text -> Either DomainError OrderId
create id = OrderIdPrivate <$> createStringInLengthRange id "order ID" 0 50

value :: OrderId -> Text
value (OrderIdPrivate id) = id
