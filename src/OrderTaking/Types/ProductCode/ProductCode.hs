{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.ProductCode.ProductCode
  ( ProductCode (..),
    create,
    value,
  )
where

import Data.Text as T (Text, head)
import OrderTaking.Shared.DomainError (DomainError)
import qualified OrderTaking.Types.ProductCode.GizmoCode as GizmoCode
import qualified OrderTaking.Types.ProductCode.WidgetCode as WidgetCode

data ProductCode = Widget WidgetCode.WidgetCode | Gizmo GizmoCode.GizmoCode deriving (Show, Eq)

create :: Text -> Either DomainError ProductCode
create code
  | start == 'G' = Gizmo <$> GizmoCode.create code
  | start == 'W' = Widget <$> WidgetCode.create code
  | otherwise = Left $ "product code " <> code <> " is invalid. must start with G or W"
  where
    start = T.head code

value :: ProductCode -> Text
value (Widget wc) = WidgetCode.value wc
value (Gizmo gc) = GizmoCode.value gc