module OrderTaking.Shared.EitherIO
  ( EitherIO (..),
    liftEither,
    fromList,
    liftIO,
  )
where

import Control.Applicative (Applicative (liftA2))

newtype EitherIO e a = EitherIO {runEitherIO :: IO (Either e a)}

instance Functor (EitherIO e) where
  fmap g = EitherIO . fmap (fmap g) . runEitherIO

instance Applicative (EitherIO e) where
  pure = EitherIO . return . Right
  fg <*> f = EitherIO $ liftA2 (<*>) (runEitherIO fg) (runEitherIO f)

instance Monad (EitherIO e) where
  m >>= mg = EitherIO $ runEitherIO m >>= either (pure . Left) (runEitherIO . mg)

-- make EitherIO from Either
liftEither :: Either e a -> EitherIO e a
liftEither = EitherIO . pure

-- make EitherIO from IO
liftIO :: IO a -> EitherIO e a
liftIO = EitherIO . fmap Right

-- convert a list of EitherIOs to a list within EitherIO
fromList :: [EitherIO e a] -> EitherIO e [a]
fromList = EitherIO . fromList'

-- private functions

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
