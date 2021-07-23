{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.ProductCode
  ( ProductCode (..),
    OrderTaking.Types.ProductCode.create,
    OrderTaking.Types.ProductCode.value,
  )
where

import Data.Text as T (Text, head)
import OrderTaking.Shared (DomainError)
import OrderTaking.Types.GizmoCode as GizmoCode
import OrderTaking.Types.WidgetCode as WidgetCode

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