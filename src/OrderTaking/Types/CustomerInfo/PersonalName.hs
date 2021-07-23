{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.CustomerInfo.PersonalName
  ( PersonalName,
    Params (..),
    create,
    value,
  )
where

import Data.Text (Text)
import OrderTaking.Shared (DomainError, createStringInLengthRange)

data PersonalName = PersonalNamePrivate
  { firstNamePrivate :: Text,
    lastNamePrivate :: Text
  }
  deriving (Show, Eq)

data Params = Params
  { firstName :: Text,
    lastName :: Text
  }
  deriving (Show, Eq)

create :: Params -> Either DomainError PersonalName
create Params {firstName = f, lastName = l} = do
  f' <- createStringInLengthRange f "first name" 0 50
  l' <- createStringInLengthRange l "last name" 0 50
  return
    PersonalNamePrivate
      { firstNamePrivate = f',
        lastNamePrivate = l'
      }

value :: PersonalName -> Params
value
  PersonalNamePrivate
    { firstNamePrivate = f,
      lastNamePrivate = l
    } =
    Params
      { firstName = f,
        lastName = l
      }