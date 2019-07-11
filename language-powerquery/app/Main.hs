{-# LANGUAGE PackageImports #-}
module Main where

import                                     Prelude hiding (lex)
import           "base"                    Data.Semigroup ((<>))
import           "base"                    System.IO (stdin)
import qualified "bytestring"              Data.ByteString.Lazy.Char8 as B (ByteString, readFile, putStrLn, hGetContents, unpack)
import           "aeson"                   Data.Aeson (encode)
import           "optparse-applicative"    Options.Applicative
import           "language-powerquery-ast" Language.PowerQuery.AST
import           "language-powerquery"     Language.PowerQuery


data Lex
    = Lex
    { _lex_path :: !(Maybe FilePath)
    , _lex_json :: !Bool
    }

data Parse
    = Parse
    { _parse_path :: !(Maybe FilePath)
    , _parse_json :: !Bool
    }

data PPrint
    = PPrint
    { _print_path :: !(Maybe FilePath)
    }

data Command
    = C1 Lex
    | C2 Parse
    | C3 PPrint

lexParser :: Parser Command
lexParser = C1 <$> (Lex
    <$> optional (strOption
      ( long "path"
     <> short 'p'
     <> metavar "PATH"
     <> help "Lex PowerQuery Text"
      ))
    <*> switch
      ( long "json"
     <> short 'j'
     <> help "print Tokens as JSON"
      )
    )

parseParser :: Parser Command
parseParser = C2 <$> (Parse
    <$> optional (strOption
      ( long "path"
     <> short 'p'
     <> metavar "PATH"
     <> help "Parse PowerQuery Text"
      ))
    <*> switch
      ( long "json"
     <> short 'j'
     <> help "print AST as JSON"
      )
    )

combined :: Parser Command
combined = subparser
         ( command "lex" (info lexParser (progDesc "Lex PowerQuery Text"))
        <> command "parse" (info parseParser (progDesc "Parse PowerQuery Text"))
         )

fromRight (Right x)  = x
fromRight (Left err) = error err


readPathOrStdin :: Maybe FilePath -> IO B.ByteString
readPathOrStdin Nothing     = B.hGetContents stdin
readPathOrStdin (Just path) = B.readFile path


lex :: Lex -> IO ()
lex (Lex mPath shouldJSON) = do
    bs <- readPathOrStdin mPath
    let tokens = lexer
               . B.unpack
               $ bs
    if shouldJSON
    then B.putStrLn . encode $ tokens
    else print               $ tokens
    return ()


parse :: Parse -> IO ()
parse (Parse mPath shouldJSON) = do
    bs <- readPathOrStdin mPath
    let ast = parseDocument
            . fromRight
            . lexer
            . B.unpack
            $ bs
    if shouldJSON
    then B.putStrLn . encode $ ast
    else print               $ ast
    return ()


main :: IO ()
main = do
    options <- execParser (info (combined <**> helper) (fullDesc <> progDesc "PowerQuery Lexer/Parser/Pretty Printer"))
    case options of
        C1 config -> lex config
        C2 config -> parse config
        C3 config -> undefined
