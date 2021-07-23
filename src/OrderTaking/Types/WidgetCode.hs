{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.WidgetCode
  ( WidgetCode,
    create,
    value,
  )
where

import Data.Text (Text)
import OrderTaking.Shared (DomainError, createStringMatchedToPattern)

newtype WidgetCode = WidgetCodePrivate Text deriving (Show, Eq)

create :: Text -> Either DomainError WidgetCode
create c = WidgetCodePrivate <$> createStringMatchedToPattern c "widget code" "W[0-9]{4}" "Wxxxx"

value :: WidgetCode -> Text
value (WidgetCodePrivate c) = c
