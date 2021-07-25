{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.CustomerInfo.CustomerInfo
  ( CustomerInfo(..)
  ) where

import           GHC.Generics                   ( Generic )
import qualified OrderTaking.Types.CustomerInfo.EmailAddress
                                               as EmailAddress
import qualified OrderTaking.Types.CustomerInfo.PersonalName
                                               as PersonalName


data CustomerInfo = CustomerInfo
  { personalName :: PersonalName.PersonalName
  , emailAddress :: EmailAddress.EmailAddress
  }
  deriving (Show, Eq, Generic)
