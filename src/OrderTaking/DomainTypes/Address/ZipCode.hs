{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.DomainTypes.Address.ZipCode
  ( ZipCode
  , create
  , value
  ) where

import           Data.Text                      ( Text )
import           OrderTaking.Shared.DomainError ( DomainError )
import           OrderTaking.Shared.UtilFunctions
                                                ( createStringMatchedToPattern )


newtype ZipCode = ZipCodePrivate Text deriving (Show, Eq)

create :: Text -> Either DomainError ZipCode
create c = ZipCodePrivate
  <$> createStringMatchedToPattern c "zip code" "[0-9]{5}" "5 digits number"

value :: ZipCode -> Text
value (ZipCodePrivate c) = c
