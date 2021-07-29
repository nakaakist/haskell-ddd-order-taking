{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.DomainTypes.BillingAmount
  ( BillingAmount
  , create
  , value
  , sumPrices
  , shouldBeBilled
  ) where

import qualified OrderTaking.DomainTypes.Price as Price
import           OrderTaking.Shared.DomainError ( DomainError )
import           OrderTaking.Shared.UtilFunctions
                                                ( createNumInRange )


newtype BillingAmount = BillingAmountPrivate Double deriving (Show, Eq)

create :: Double -> Either DomainError BillingAmount
create a =
  BillingAmountPrivate <$> createNumInRange a "billing amount" 0.0 10000.0

value :: BillingAmount -> Double
value (BillingAmountPrivate p) = p

sumPrices :: [Price.Price] -> Either DomainError BillingAmount
sumPrices ps = let total = sum $ Price.value <$> ps in create total

shouldBeBilled :: BillingAmount -> Bool
shouldBeBilled (BillingAmountPrivate p) = p > 0
