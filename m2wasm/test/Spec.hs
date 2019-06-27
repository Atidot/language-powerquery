{-# LANGUAGE PackageImports #-}
module Main where

import           "hspec"  Test.Hspec hiding (example)
import qualified "m2wasm" Atidot.Compiler.M2Wasm.Test as M2Wasm


main :: IO ()
main = hspec $ do
    return ()
