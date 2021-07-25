{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.OrderQuantity.UnitQuantity
  ( UnitQuantity
  , create
  , value
  ) where

import           OrderTaking.Shared.DomainError ( DomainError )
import           OrderTaking.Shared.UtilFunctions
                                                ( createNumInRange )


newtype UnitQuantity = UnitQuantityPrivate Int deriving (Show, Eq)

create :: Int -> Either DomainError UnitQuantity
create q = UnitQuantityPrivate <$> createNumInRange q "unit quantity" 1 1000

value :: UnitQuantity -> Int
value (UnitQuantityPrivate q) = q
