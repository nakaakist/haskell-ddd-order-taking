{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.PersonalName
  ( PersonalName,
    PersonalNameDto,
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

data PersonalNameDto = PersonalNameDto
  { firstName :: Text,
    lastName :: Text
  }
  deriving (Show, Eq)

create :: PersonalNameDto -> Either DomainError PersonalName
create PersonalNameDto {firstName = f, lastName = l} = do
  f' <- createStringInLengthRange f "first name" 0 50
  l' <- createStringInLengthRange l "last name" 0 50
  return
    PersonalNamePrivate
      { firstNamePrivate = f',
        lastNamePrivate = l'
      }

value :: PersonalName -> PersonalNameDto
value
  PersonalNamePrivate
    { firstNamePrivate = f,
      lastNamePrivate = l
    } =
    PersonalNameDto
      { firstName = f,
        lastName = l
      }