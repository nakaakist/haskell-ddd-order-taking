{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.Address
  ( Address,
    AddressDto,
    create,
    value,
  )
where

import Data.Text (Text)
import OrderTaking.Shared (DomainError, createStringInLengthRange)
import qualified OrderTaking.Types.ZipCode as ZipCode

data Address = AddressPrivate
  { addressLine1Private :: Text,
    addressLine2Private :: Text,
    addressLine3Private :: Text,
    addressLine4Private :: Text,
    cityPrivate :: Text,
    zipCodePrivate :: ZipCode.ZipCode
  }
  deriving (Show, Eq)

data AddressDto = AddressDto
  { addressLine1 :: Text,
    addressLine2 :: Text,
    addressLine3 :: Text,
    addressLine4 :: Text,
    city :: Text,
    zipCode :: Text
  }
  deriving (Show, Eq)

create :: AddressDto -> Either DomainError Address
create
  AddressDto
    { addressLine1 = a1,
      addressLine2 = a2,
      addressLine3 = a3,
      addressLine4 = a4,
      city = c,
      zipCode = z
    } = do
    a1' <- createStringInLengthRange a1 "address line 1" 0 50
    a2' <- createStringInLengthRange a2 "address line 2" 0 50
    a3' <- createStringInLengthRange a3 "address line 3" 0 50
    a4' <- createStringInLengthRange a4 "address line 4" 0 50
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

value :: Address -> AddressDto
value
  AddressPrivate
    { addressLine1Private = a1,
      addressLine2Private = a2,
      addressLine3Private = a3,
      addressLine4Private = a4,
      cityPrivate = c,
      zipCodePrivate = z
    } =
    AddressDto
      { addressLine1 = a1,
        addressLine2 = a2,
        addressLine3 = a3,
        addressLine4 = a4,
        city = c,
        zipCode = ZipCode.value z
      }