{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.Address.Address
  ( Address,
    Params (..),
    create,
    value,
  )
where

import Data.Text (Text)
import OrderTaking.Shared
  ( DomainError,
    createMaybeStringInLengthRange,
    createStringInLengthRange,
    valueFromMaybeString,
  )
import qualified OrderTaking.Types.Address.ZipCode as ZipCode

data Address = AddressPrivate
  { addressLine1Private :: Text,
    addressLine2Private :: Maybe Text,
    addressLine3Private :: Maybe Text,
    addressLine4Private :: Maybe Text,
    cityPrivate :: Text,
    zipCodePrivate :: ZipCode.ZipCode
  }
  deriving (Show, Eq)

data Params = Params
  { addressLine1 :: Text,
    addressLine2 :: Text,
    addressLine3 :: Text,
    addressLine4 :: Text,
    city :: Text,
    zipCode :: Text
  }
  deriving (Show, Eq)

create :: Params -> Either DomainError Address
create
  Params
    { addressLine1 = a1,
      addressLine2 = a2,
      addressLine3 = a3,
      addressLine4 = a4,
      city = c,
      zipCode = z
    } = do
    a1' <- createStringInLengthRange a1 "address line 1" 0 50
    a2' <- createMaybeStringInLengthRange a2 "address line 2" 0 50
    a3' <- createMaybeStringInLengthRange a3 "address line 3" 0 50
    a4' <- createMaybeStringInLengthRange a4 "address line 4" 0 50
    c' <- createStringInLengthRange c "city" 0 50
    z' <- ZipCode.create z
    return
      AddressPrivate
        { addressLine1Private = a1',
          addressLine2Private = a2',
          addressLine3Private = a3',
          addressLine4Private = a4',
          cityPrivate = c',
          zipCodePrivate = z'
        }

value :: Address -> Params
value
  AddressPrivate
    { addressLine1Private = a1,
      addressLine2Private = a2,
      addressLine3Private = a3,
      addressLine4Private = a4,
      cityPrivate = c,
      zipCodePrivate = z
    } =
    Params
      { addressLine1 = a1,
        addressLine2 = valueFromMaybeString a2,
        addressLine3 = valueFromMaybeString a3,
        addressLine4 = valueFromMaybeString a4,
        city = c,
        zipCode = ZipCode.value z
      }