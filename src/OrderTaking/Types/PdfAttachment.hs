{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.PdfAttachment
  ( PdfAttachment
  ) where

import           Data.ByteString                ( ByteString )
import           Data.Text                      ( Text )


data PdfAttachment = PdfAttachment
  { name  :: Text
  , bytes :: ByteString
  }
