{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.OrderQuantity
  ( OrderQuantity,
    OrderTaking.Types.OrderQuantity.create,
    OrderTaking.Types.OrderQuantity.value,
  )
where

import Data.Text (Text)
import OrderTaking.Shared (DomainError)
import OrderTaking.Types.KilogramQuantity as KilogramQuantity
import OrderTaking.Types.ProductCode as ProductCode
import OrderTaking.Types.UnitQuantity as UnitQuantity

data OrderQuantity = Unit UnitQuantity.UnitQuantity | Kilo KilogramQuantity.KilogramQuantity deriving (Show, Eq)

create :: ProductCode.ProductCode -> Double -> Either DomainError OrderQuantity
create (Widget _) q = Unit <$> UnitQuantity.create (floor q)
create (Gizmo _) q = Kilo <$> KilogramQuantity.create q

value :: OrderQuantity -> Double
value (Unit q) = fromIntegral $ UnitQuantity.value q
value (Kilo q) = KilogramQuantity.value q