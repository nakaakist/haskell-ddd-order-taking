{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.Price
  ( Price (..),
    create,
    value,
    multiply,
  )
where

import OrderTaking.Shared.DomainError (DomainError)
import OrderTaking.Shared.UtilFunctions (createNumInRange)

newtype Price = PricePrivate Double deriving (Show, Eq)

create :: Double -> Either DomainError Price
create p = PricePrivate <$> createNumInRange p "price" 0.0 1000.0

value :: Price -> Double
value (PricePrivate p) = p

multiply :: Double -> Price -> Either DomainError Price
multiply x p = create $ value p * x
