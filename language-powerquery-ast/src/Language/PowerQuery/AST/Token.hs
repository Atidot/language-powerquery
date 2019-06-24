{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Language.PowerQuery.AST.Token where

import "base" GHC.Generics  (Generic)
import "base" Data.Typeable (Typeable)
import "base" Data.Data     (Data)
import "text" Data.Text     (Text)

-- 12.1.3 Tokens
data Token
    = TWhitespace
    | TComment
    | TLiteral    Literal
    | TIdentifier Identifier
    | TKeyword    Keyword
    | TOperator   Operator
    | TEOF
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.1.5 Literals
data Literal
    = Logical' Bool
    | Integer' Integer
    | Float'   Float
    | String'  Text
    | Null
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.1.6 Identifiers
data Identifier
    = RegularIdentifier Text
    | QuotedIdentifier Text
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.1.7 Keywords and predefined identifiers
data Keyword
    = And
    | As
    | Each
    | Else
    | Error
    | False'
    | If
    | In
    | Is
    | Let
    | Meta
    | Not
    | Otherwise
    | Or
    | Section
    | Shared
    | Then
    | True'
    | Try
    | Type'
    | H_Binary
    | H_Date
    | H_DateTime
    | H_DateTimezone
    | H_Duration
    | H_Infinity
    | H_Nan
    | H_Sections
    | H_Shared
    | H_Table
    | H_Time
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

keywords :: [Keyword]
keywords = [toEnum 0 .. ]


-- 12.1.8 Operators and punctuators
data Operator
    = Comma
    | SemiColon
    | Equal
    | LT'
    | LEQ
    | GT'
    | GEQ
    | NEQ
    | Plus
    | Minus
    | Mult
    | Div
    | Ampersand
    | LeftParen
    | RightParen
    | LeftBracket
    | RightBracket
    | LeftCurly
    | RightCurly
    | At
    | QMark
    | Arrow
    | TwoDots
    | ThreeDots
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

operators :: [Operator]
operators = [toEnum 0 .. ]
