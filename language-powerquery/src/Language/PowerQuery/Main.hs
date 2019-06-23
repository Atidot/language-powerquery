{-# LANGUAGE PackageImports #-}
module Language.PowerQuery.Main where

import "base" System.IO (stdin, hGetContents)
import        Language.PowerQuery.Lexer  (lexer, runAlex)
import        Language.PowerQuery.Parser

main :: IO ()
main = do
    s <- hGetContents stdin
    case lexer s of
        Left err -> print err
        Right tokens -> do
            print tokens
            let result = parseExpression tokens
            print result
    --let ts = runAlex s $ powerquery 
    return ()
