{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.CustomerInfo.EmailAddress
  ( EmailAddress
  , create
  , value
  ) where

import           Data.Either                    ( fromLeft
                                                , isRight
                                                )
import           Data.Text                      ( Text
                                                , pack
                                                )
import           Data.Text.Encoding             ( encodeUtf8 )
import           OrderTaking.Shared.DomainError ( DomainError )
import           Text.Email.Validate            ( validate )


newtype EmailAddress = EmailAddressPrivate Text deriving (Show, Eq)

create :: Text -> Either DomainError EmailAddress
create e
  | isRight validationResult = Right $ EmailAddressPrivate e
  | otherwise = Left $ "email address " <> e <> " is invalid. " <> pack
    (fromLeft "" validationResult)
  where validationResult = validate $ encodeUtf8 e

value :: EmailAddress -> Text
value (EmailAddressPrivate e) = e
