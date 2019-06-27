{-# LANGUAGE PackageImports #-}
module Codec.Pbix.Types.Types where

import "bytestring"  Data.ByteString.Lazy.Char8 (ByteString)
import "zip-archive" Codec.Archive.Zip (Archive)

newtype Pbix = Pbix Archive
    deriving (Show, Read)

newtype DataMashup = DataMashup Archive
    deriving (Show, Read)

newtype Formula = Formula ByteString
    deriving (Show, Read)
