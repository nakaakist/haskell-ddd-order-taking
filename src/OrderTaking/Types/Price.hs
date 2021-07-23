{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.Price
  ( create,
    value,
  )
where

import OrderTaking.Shared (DomainError, createNumInRange)

newtype Price = PricePrivate Double deriving (Show, Eq)

create :: Double -> Either DomainError Price
create p = PricePrivate <$> createNumInRange p "price" 0.0 1000.0

value :: Price -> Double
value (PricePrivate p) = p
