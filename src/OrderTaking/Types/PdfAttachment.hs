{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE OverloadedStrings #-}

module OrderTaking.Types.PdfAttachment
  ( PdfAttachment
  ) where

import           Data.ByteString                ( ByteString )
import           Data.Text                      ( Text )
import           GHC.Generics                   ( Generic )


data PdfAttachment = PdfAttachment
  { name  :: Text
  , bytes :: ByteString
  }
  deriving (Show, Eq, Generic)

