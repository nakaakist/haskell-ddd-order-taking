module OrderTaking.Types.CustomerInfo
  ( CustomerInfo,
  )
where

import qualified OrderTaking.Types.EmailAddress as EmailAddress
import qualified OrderTaking.Types.PersonalName as PersonalName

data CustomerInfo = CustomerInfo
  { personalName :: PersonalName.PersonalName,
    emailAddress :: EmailAddress.EmailAddress
  }
  deriving (Show, Eq)
