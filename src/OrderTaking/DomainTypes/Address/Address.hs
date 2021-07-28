{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.DomainTypes.Address.Address
  ( Address
  , Params(..)
  , create
  , value
  ) where

import           Data.Aeson                     ( FromJSON
                                                , ToJSON
                                                )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )
import qualified OrderTaking.DomainTypes.Address.ZipCode
                                               as ZipCode
import           OrderTaking.Shared.DomainError ( DomainError )
import           OrderTaking.Shared.UtilFunctions
                                                ( createMaybeStringInLengthRange
                                                , createStringInLengthRange
                                                , valueFromMaybeString
                                                )

data Address = AddressPrivate
  { addressLine1 :: Text
  , addressLine2 :: Maybe Text
  , addressLine3 :: Maybe Text
  , addressLine4 :: Maybe Text
  , city         :: Text
  , zipCode      :: ZipCode.ZipCode
  }
  deriving (Show, Eq)

data Params = Params
  { addressLine1 :: Text
  , addressLine2 :: Text
  , addressLine3 :: Text
  , addressLine4 :: Text
  , city         :: Text
  , zipCode      :: Text
  }
  deriving (Show, Eq, Generic)
instance ToJSON Params
instance FromJSON Params

create :: Params -> Either DomainError Address
create Params { addressLine1 = a1, addressLine2 = a2, addressLine3 = a3, addressLine4 = a4, city = c, zipCode = z }
  = do
    a1' <- createStringInLengthRange a1 "address line 1" 0 50
    a2' <- createMaybeStringInLengthRange a2 "address line 2" 0 50
    a3' <- createMaybeStringInLengthRange a3 "address line 3" 0 50
    a4' <- createMaybeStringInLengthRange a4 "address line 4" 0 50
    c'  <- createStringInLengthRange c "city" 0 50
    z'  <- ZipCode.create z
    return AddressPrivate { addressLine1 = a1'
                          , addressLine2 = a2'
                          , addressLine3 = a3'
                          , addressLine4 = a4'
                          , city         = c'
                          , zipCode      = z'
                          }

value :: Address -> Params
value AddressPrivate { addressLine1 = a1, addressLine2 = a2, addressLine3 = a3, addressLine4 = a4, city = c, zipCode = z }
  = Params { addressLine1 = a1
           , addressLine2 = valueFromMaybeString a2
           , addressLine3 = valueFromMaybeString a3
           , addressLine4 = valueFromMaybeString a4
           , city         = c
           , zipCode      = ZipCode.value z
           }
