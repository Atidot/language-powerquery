{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TypeSynonymInstances #-}
module Codec.Pbix.Main where

import           "base"        System.Environment (getArgs)
import           "lens"        Control.Lens
import qualified "bytestring"  Data.ByteString.Lazy.Char8 as BL8
import qualified "text"        Data.Text.Encoding as T
import           "zip-archive" Codec.Archive.Zip
import                         Codec.Pbix.Types
import                         Language.PowerQuery.AST


main :: IO ()
main = do
    (path : _) <- getArgs
    bs <- BL8.readFile path
    print $ bs ^. pbix . dataMashup . formula "Section1.m"
    print $ bs ^. pbix . dataMashup . formula "Section1.m" . document
