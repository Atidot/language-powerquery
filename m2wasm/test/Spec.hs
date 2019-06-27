{-# LANGUAGE PackageImports #-}
module Main where

import "hspec"  Test.Hspec hiding (example)
import "m2wasm" Atidot.Compiler.M2Wasm.Test


main :: IO ()
main = hspec $ do
    return ()
