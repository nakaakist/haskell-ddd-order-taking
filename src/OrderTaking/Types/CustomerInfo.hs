module OrderTaking.Types.CustomerInfo
  ( CustomerInfo,
  )
where

import OrderTaking.Types.EmailAddress as EmailAddress
import OrderTaking.Types.PersonalName as PersonalName

data CustomerInfo = CustomerInfo
  { personalName :: PersonalName.PersonalName,
    emailAddress :: EmailAddress.EmailAddress
  }
  deriving (Show, Eq)
