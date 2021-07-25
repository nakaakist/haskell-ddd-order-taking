{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.OrderQuantity.KilogramQuantity
  ( KilogramQuantity
  , create
  , value
  ) where

import           OrderTaking.Shared.DomainError ( DomainError )
import           OrderTaking.Shared.UtilFunctions
                                                ( createNumInRange )


newtype KilogramQuantity = KilogramQuantityPrivate Double deriving (Show, Eq)

create :: Double -> Either DomainError KilogramQuantity
create a =
  KilogramQuantityPrivate <$> createNumInRange a "kilogram quantity" 0.5 100.0

value :: KilogramQuantity -> Double
value (KilogramQuantityPrivate q) = q
