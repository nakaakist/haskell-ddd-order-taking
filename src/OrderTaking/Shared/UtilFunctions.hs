{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Shared.UtilFunctions
  ( createStringInLengthRange
  , createMaybeStringInLengthRange
  , valueFromMaybeString
  , createStringMatchedToPattern
  , createNumInRange
  ) where

import           Data.Text                     as T
                                                ( Text
                                                , length
                                                , pack
                                                )
import           OrderTaking.Shared.DomainError ( DomainError )
import           Text.Regex.TDFA                ( (=~) )


type FieldName = Text

createStringInLengthRange
  :: Text -> FieldName -> Int -> Int -> Either DomainError Text
createStringInLengthRange text fieldName minLen maxLen
  | len >= minLen && len <= maxLen
  = Right text
  | otherwise
  = Left
    $  fieldName
    <> " "
    <> text
    <> " is invalid. length must be between "
    <> toText minLen
    <> " and "
    <> toText maxLen
  where len = T.length text

createMaybeStringInLengthRange
  :: Text -> FieldName -> Int -> Int -> Either DomainError (Maybe Text)
createMaybeStringInLengthRange text fieldName minLen maxLen
  | len == 0
  = Right Nothing
  | len >= minLen && len <= maxLen
  = Right $ Just text
  | otherwise
  = Left
    $  fieldName
    <> " "
    <> text
    <> " is invalid. length must be 0 or between "
    <> toText minLen
    <> " and "
    <> toText maxLen
  where len = T.length text

valueFromMaybeString :: Maybe Text -> Text
valueFromMaybeString (Just t) = t
valueFromMaybeString Nothing  = ""

type Pattern = Text

type PatternDescription = Text

createStringMatchedToPattern
  :: Text
  -> FieldName
  -> Pattern
  -> PatternDescription
  -> Either DomainError Text
createStringMatchedToPattern text fieldName pattern patternDescription
  | text =~ pattern
  = Right text
  | otherwise
  = Left
    $  fieldName
    <> " "
    <> text
    <> " is invalid. must be "
    <> patternDescription

createNumInRange
  :: (Show a, Num a, Ord a) => a -> FieldName -> a -> a -> Either DomainError a
createNumInRange num fieldName minVal maxVal
  | num >= minVal && num <= maxVal
  = Right num
  | otherwise
  = Left
    $  fieldName
    <> " "
    <> toText num
    <> " is invalid. must be between "
    <> toText minVal
    <> " and "
    <> toText maxVal

toText :: (Show a) => a -> Text
toText = pack . show
