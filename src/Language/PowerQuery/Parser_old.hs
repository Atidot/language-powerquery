{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.PowerQuery.Parser where

import "base"       Control.Applicative
import "base"       Data.Char (isSpace)
import "text"       Data.Text (Text)
import "attoparsec" Data.Attoparsec.Text
import              Language.PowerQuery.Token
--
import           "base"  System.Environment (getArgs)
import           "base"  System.IO (stdin)
import qualified "text"  Data.Text.IO as T (hGetContents)

{-
-- 12.1 Lexical grammar
lexicalUnit :: Parser [Token]
lexicalUnit = do
    ts <- many' lexicalElement
    endOfInput
    return ts

lexicalElement :: Parser Token
lexicalElement = whitespace
             <|> token
             <|> comment

-- 12.1.1 White space
whitespace :: Parser Token
whitespace = do
    _ <- takeWhile1 isSpace
    return $ TWhitespace

-- 12.1.2 Comment
comment :: Parser Token
comment = singleLineComment
      <|> delimitedComment
    where
        singleLineComment :: Parser Token
        singleLineComment = do
            string "//"
            comment <- takeTill isEndOfLine
            return $ TComment comment

        delimitedComment :: Parser Token
        delimitedComment = do
            string "/*"
            comment <- string "/"
            asterisks
            string "/"
            return $ TComment comment
            where
                asterisks :: Parser Text
                asterisks = takeWhile1 (== '*')

-- 12.1.3 Tokens
token :: Parser Token
token = {-(TIdentifier <$> identifier)-}
    {-<|> -}(TKeyword    <$> keyword)
    <|> (TLiteral    <$> literal)
    <|> (TOperator   <$> operatorOrPunctuator)

-- 12.1.5 Literals
literal :: Parser Literal
literal
    = numberLiteral
  <|> textLiteral
  <|> nullLiteral
    where
        numberLiteral :: Parser Literal
        numberLiteral
            = hexadecimalNumberLiteral
          <|> decimalNumberLiteral

        decimalNumberLiteral :: Parser Literal
        decimalNumberLiteral = do
            number <- decimal -- TODO: fix for Floats Double, etc
            return $ Number number

        hexadecimalNumberLiteral :: Parser Literal
        hexadecimalNumberLiteral = do
            string "0x"
            (number :: Integer) <- hexadecimal
            return $ Number number

        textLiteral :: Parser Literal
        textLiteral = do
            string "\""
            text <- undefined
            string "\""
            return $ Text' text

        nullLiteral :: Parser Literal
        nullLiteral = do
            string "null"
            return $ Null

-- 12.1.6 Identifiers
identifier :: Parser Identifier
identifier = undefined

-- 12.1.7 Keyword and predefined identifiers
keyword :: Parser Keyword
keyword = do
    return And
    --value <- choice . map string $ keywords
    --let index = elemIndex value keywords
    --return $ fromEnum index
    value <- choice $ string <$>
           [ "and"
           , "as"
           , "each"
           , "else"
           , "error"
           , "false"
           , "if"
           , "in"
           , "is"
           , "let"
           , "meta"
           , "not"
           , "otherwise"
           , "or"
           , "section"
           , "shared"
           , "then"
           , "true"
           , "try"
           , "type"
           , "#binary"
           , "#date"
           , "#datetime"
           , "#datetimezone"
           , "#duration"
           , "#infinity"
           , "#nan"
           , "#sections"
           , "#shared"
           , "#table"
           , "#time"
           ]
    return And

-- 12.1.8 Operators and punctuators
operatorOrPunctuator :: Parser Operator
operatorOrPunctuator = do
    --return Comma
    value <- choice $ string <$>
           [ ","
           , ";"
           , "="
           , "<"
           , "<="
           , ">"
           , ">="
           , "<>"
           , "+"
           , "-"
           , "*"
           , "/"
           , "&"
           , "("
           , ")"
           , "["
           , "]"
           , "{"
           , "}"
           , "@"
           , "?"
           , "=>"
           , ".."
           , "..."
           ]
    return Comma

main :: IO ()
main = do
    text <- T.hGetContents stdin
    print text
    case parseOnly lexicalUnit text of
        Left err -> print err
        Right tokens -> do
            print tokens
-}
