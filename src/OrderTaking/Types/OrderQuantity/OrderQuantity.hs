{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.OrderQuantity.OrderQuantity
  ( OrderQuantity
  , create
  , value
  ) where

import           OrderTaking.Shared.DomainError ( DomainError )
import qualified OrderTaking.Types.OrderQuantity.KilogramQuantity
                                               as KilogramQuantity
import qualified OrderTaking.Types.OrderQuantity.UnitQuantity
                                               as UnitQuantity
import qualified OrderTaking.Types.ProductCode.ProductCode
                                               as ProductCode


data OrderQuantity = Unit UnitQuantity.UnitQuantity | Kilo KilogramQuantity.KilogramQuantity deriving (Show, Eq)

create :: ProductCode.ProductCode -> Double -> Either DomainError OrderQuantity
create (ProductCode.Widget _) q = Unit <$> UnitQuantity.create (floor q)
create (ProductCode.Gizmo  _) q = Kilo <$> KilogramQuantity.create q

value :: OrderQuantity -> Double
value (Unit q) = fromIntegral $ UnitQuantity.value q
value (Kilo q) = KilogramQuantity.value q
