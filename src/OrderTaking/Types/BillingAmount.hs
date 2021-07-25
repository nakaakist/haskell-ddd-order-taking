{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.BillingAmount
  ( BillingAmount(..)
  , create
  , value
  , sumPrices
  ) where

import           OrderTaking.Shared.DomainError ( DomainError )
import           OrderTaking.Shared.UtilFunctions
                                                ( createNumInRange )
import qualified OrderTaking.Types.Price       as Price


newtype BillingAmount = BillingAmountPrivate Double deriving (Show, Eq)

create :: Double -> Either DomainError BillingAmount
create a =
  BillingAmountPrivate <$> createNumInRange a "billing amount" 0.0 10000.0

value :: BillingAmount -> Double
value (BillingAmountPrivate p) = p

sumPrices :: [Price.Price] -> Either DomainError BillingAmount
sumPrices ps = let total = sum $ Price.value <$> ps in create total
