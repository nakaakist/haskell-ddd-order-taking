{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.DomainTypes.ProductCode.GizmoCode
  ( GizmoCode
  , create
  , value
  ) where

import           Data.Text                      ( Text )
import           OrderTaking.Shared.DomainError ( DomainError )
import           OrderTaking.Shared.UtilFunctions
                                                ( createStringMatchedToPattern )


newtype GizmoCode = GizmoCodePrivate Text deriving (Show, Eq)

create :: Text -> Either DomainError GizmoCode
create c = GizmoCodePrivate
  <$> createStringMatchedToPattern c "gizmo code" "G[0-9]{3}" "Gxxx"

value :: GizmoCode -> Text
value (GizmoCodePrivate c) = c
