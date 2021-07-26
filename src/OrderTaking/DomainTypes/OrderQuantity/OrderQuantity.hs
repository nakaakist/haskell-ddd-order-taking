{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.DomainTypes.OrderQuantity.OrderQuantity
  ( OrderQuantity
  , create
  , value
  ) where

import qualified OrderTaking.DomainTypes.OrderQuantity.KilogramQuantity
                                               as KilogramQuantity
import qualified OrderTaking.DomainTypes.OrderQuantity.UnitQuantity
                                               as UnitQuantity
import qualified OrderTaking.DomainTypes.ProductCode.ProductCode
                                               as ProductCode
import           OrderTaking.Shared.DomainError ( DomainError )


data OrderQuantity = Unit UnitQuantity.UnitQuantity | Kilo KilogramQuantity.KilogramQuantity deriving (Show, Eq)

create :: ProductCode.ProductCode -> Double -> Either DomainError OrderQuantity
create (ProductCode.Widget _) q = Unit <$> UnitQuantity.create (floor q)
create (ProductCode.Gizmo  _) q = Kilo <$> KilogramQuantity.create q

value :: OrderQuantity -> Double
value (Unit q) = fromIntegral $ UnitQuantity.value q
value (Kilo q) = KilogramQuantity.value q
