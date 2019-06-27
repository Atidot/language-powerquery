{-# LANGUAGE PackageImports #-}
module Main where

import "hspec"  Test.Hspec hiding (example)
import "pbix"   Codec.Pbix.Types


main :: IO ()
main = hspec $ do
    return ()
