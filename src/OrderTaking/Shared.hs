{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Shared
  ( DomainError,
    EitherIO (..),
    liftEither,
    fromList,
    liftIO,
    createStringInLengthRange,
    createMaybeStringInLengthRange,
    valueFromMaybeString,
    createStringMatchedToPattern,
    createNumInRange,
  )
where

import Control.Applicative (Applicative (liftA2))
import Data.Either (lefts)
import Data.Text as T (Text, length, pack)
import Text.Regex.TDFA ((=~))

type DomainError = Text

-- EitherIO type
newtype EitherIO e a = EitherIO {runEitherIO :: IO (Either e a)}

instance Functor (EitherIO e) where
  fmap f = EitherIO . fmap (fmap f) . runEitherIO

instance Applicative (EitherIO e) where
  pure = EitherIO . return . Right
  fg <*> f = EitherIO $ liftA2 (<*>) (runEitherIO fg) (runEitherIO f)

instance Monad (EitherIO e) where
  m >>= mg = EitherIO $ runEitherIO m >>= either (pure . Left) (runEitherIO . mg)

liftEither :: Either e a -> EitherIO e a
liftEither = EitherIO . pure

liftIO :: IO a -> EitherIO e a
liftIO = EitherIO . fmap Right

fromList' :: [EitherIO e a] -> IO (Either e [a])
fromList' [] = return $ Right []
fromList' (x : xs) = do
  x' <- runEitherIO x
  case x' of
    (Left err) -> return $ Left err
    (Right x'') -> do
      xs' <- fromList' xs
      case xs' of
        (Left errs) -> return $ Left errs
        (Right xs'') -> return $ Right (x'' : xs'')

fromList :: [EitherIO e a] -> EitherIO e [a]
fromList = EitherIO . fromList'

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