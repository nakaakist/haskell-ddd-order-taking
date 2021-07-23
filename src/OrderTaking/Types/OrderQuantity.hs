{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.OrderQuantity
  ( OrderQuantity,
    OrderTaking.Types.OrderQuantity.create,
    OrderTaking.Types.OrderQuantity.value,
  )
where

import Data.Text (Text)
import OrderTaking.Shared (DomainError)
import qualified OrderTaking.Types.KilogramQuantity as KilogramQuantity
import qualified OrderTaking.Types.ProductCode as ProductCode
import qualified OrderTaking.Types.UnitQuantity as UnitQuantity

data OrderQuantity = Unit UnitQuantity.UnitQuantity | Kilo KilogramQuantity.KilogramQuantity deriving (Show, Eq)

create :: ProductCode.ProductCode -> Double -> Either DomainError OrderQuantity
create (ProductCode.Widget _) q = Unit <$> UnitQuantity.create (floor q)
create (ProductCode.Gizmo _) q = Kilo <$> KilogramQuantity.create q

value :: OrderQuantity -> Double
value (Unit q) = fromIntegral $ UnitQuantity.value q
value (Kilo q) = KilogramQuantity.value q