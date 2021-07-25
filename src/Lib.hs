{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( main
  ) where

import qualified Data.Aeson
import           GHC.Generics
import           Web.Scotty

data Payload = Payload
  { a :: String
  }
  deriving Generic

instance Data.Aeson.ToJSON Payload

main :: IO ()
main = scotty 3000 $ get "/" $ json $ Payload { a = "test" }
