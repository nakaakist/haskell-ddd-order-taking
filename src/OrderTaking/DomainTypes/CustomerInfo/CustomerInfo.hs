{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.DomainTypes.CustomerInfo.CustomerInfo
  ( CustomerInfo(..)
  ) where

import           GHC.Generics                   ( Generic )
import qualified OrderTaking.DomainTypes.CustomerInfo.EmailAddress
                                               as EmailAddress
import qualified OrderTaking.DomainTypes.CustomerInfo.PersonalName
                                               as PersonalName


data CustomerInfo = CustomerInfo
  { personalName :: PersonalName.PersonalName
  , emailAddress :: EmailAddress.EmailAddress
  }
  deriving (Show, Eq, Generic)
