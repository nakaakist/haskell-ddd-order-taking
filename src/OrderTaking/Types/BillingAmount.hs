{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.BillingAmount
  ( create,
    value,
  )
where

import OrderTaking.Shared (DomainError, createNumInRange)

newtype BillingAmount = BillingAmountPrivate Double deriving (Show, Eq)

create :: Double -> Either DomainError BillingAmount
create a = BillingAmountPrivate <$> createNumInRange a "billing amount" 0.0 10000.0

value :: BillingAmount -> Double
value (BillingAmountPrivate p) = p
