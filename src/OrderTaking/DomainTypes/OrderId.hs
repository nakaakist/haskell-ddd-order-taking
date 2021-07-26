{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.DomainTypes.OrderId
  ( OrderId
  , create
  , value
  ) where

import           Data.Text                      ( Text )
import           OrderTaking.Shared.DomainError ( DomainError )
import           OrderTaking.Shared.UtilFunctions
                                                ( createStringInLengthRange )


newtype OrderId = OrderIdPrivate Text deriving (Show, Eq)

create :: Text -> Either DomainError OrderId
create i = OrderIdPrivate <$> createStringInLengthRange i "order ID" 0 50

value :: OrderId -> Text
value (OrderIdPrivate i) = i
