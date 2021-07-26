{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.DomainTypes.ProductCode.ProductCode
  ( ProductCode(..)
  , create
  , value
  ) where

import           Data.Text                     as T
                                                ( Text
                                                , head
                                                )
import qualified OrderTaking.DomainTypes.ProductCode.GizmoCode
                                               as GizmoCode
import qualified OrderTaking.DomainTypes.ProductCode.WidgetCode
                                               as WidgetCode
import           OrderTaking.Shared.DomainError ( DomainError )


data ProductCode = Widget WidgetCode.WidgetCode | Gizmo GizmoCode.GizmoCode deriving (Show, Eq)

create :: Text -> Either DomainError ProductCode
create code
  | start == 'G'
  = Gizmo <$> GizmoCode.create code
  | start == 'W'
  = Widget <$> WidgetCode.create code
  | otherwise
  = Left $ "product code " <> code <> " is invalid. must start with G or W"
  where start = T.head code

value :: ProductCode -> Text
value (Widget wc) = WidgetCode.value wc
value (Gizmo  gc) = GizmoCode.value gc
