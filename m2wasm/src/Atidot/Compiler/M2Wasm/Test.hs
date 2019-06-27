{-# LANGUAGE PackageImports #-}
module Atidot.Compiler.M2Wasm.Test where

import qualified "wasm"                    Language.Wasm.Structure as Wasm
import           "language-powerquery-ast" Language.PowerQuery.AST

logical :: LogicalOrExpression Annotation -> Wasm.Expression
logical (And_OE andExpr) = undefined
logical _ = undefined

main :: IO ()
main = undefined
