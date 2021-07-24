module OrderTaking.Types.CustomerInfo.CustomerInfo
  ( CustomerInfo (..),
  )
where

import qualified OrderTaking.Types.CustomerInfo.EmailAddress as EmailAddress
import qualified OrderTaking.Types.CustomerInfo.PersonalName as PersonalName

data CustomerInfo = CustomerInfo
  { personalName :: PersonalName.PersonalName,
    emailAddress :: EmailAddress.EmailAddress
  }
  deriving (Show, Eq)
