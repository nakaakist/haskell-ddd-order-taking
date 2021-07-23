{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Shared
  ( DomainError,
    createStringInLengthRange,
    createMaybeStringInLengthRange,
    valueFromMaybeString,
    createStringMatchedToPattern,
    createNumInRange,
  )
where

import Data.Text as T (Text, length, pack)
import Text.Regex.TDFA ((=~))

type DomainError = Text

-- util functions

type FieldName = Text

createStringInLengthRange :: Text -> FieldName -> Int -> Int -> Either DomainError Text
createStringInLengthRange text fieldName min max
  | len >= min && len <= max = Right text
  | otherwise = Left $ fieldName <> " " <> text <> " is invalid. length must be between " <> toText min <> " and " <> toText max
  where
    len = T.length text

createMaybeStringInLengthRange :: Text -> FieldName -> Int -> Int -> Either DomainError (Maybe Text)
createMaybeStringInLengthRange text fieldName min max
  | len == 0 = Right Nothing
  | len >= min && len <= max = Right $ Just text
  | otherwise = Left $ fieldName <> " " <> text <> " is invalid. length must be 0 or between " <> toText min <> " and " <> toText max
  where
    len = T.length text

valueFromMaybeString :: Maybe Text -> Text
valueFromMaybeString (Just t) = t
valueFromMaybeString Nothing = ""

type Pattern = Text

type PatternDescription = Text

createStringMatchedToPattern :: Text -> FieldName -> Pattern -> PatternDescription -> Either DomainError Text
createStringMatchedToPattern text fieldName pattern patternDescription
  | text =~ pattern = Right text
  | otherwise = Left $ fieldName <> " " <> text <> " is invalid. must be " <> patternDescription

createNumInRange :: (Show a, Num a, Ord a) => a -> FieldName -> a -> a -> Either DomainError a
createNumInRange num fieldName min max
  | num >= min && num <= max = Right num
  | otherwise = Left $ fieldName <> " " <> toText num <> " is invalid. must be between " <> toText min <> " and " <> toText max

toText :: (Show a) => a -> Text
toText = pack . show