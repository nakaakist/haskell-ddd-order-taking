{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( main
  ) where

import qualified OrderTaking.Workflows.PlaceOrder.Api
                                               as PlaceOrderApi
import           Web.Scotty                     ( scotty )

main :: IO ()
main = scotty 3000 $ do
  PlaceOrderApi.routes
