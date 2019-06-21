{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Language.PowerQuery.Token where

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
    deriving (Show, Eq, Data, Typeable, Generic)

-- 12.1.5 Literals
data Literal
    = Integer' Integer
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
    deriving (Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

keywords :: [Keyword]
keywords = [toEnum 0 .. ]

instance Show Keyword where
    show And       = "and"
    show As        = "as"
    show Each      = "each"
    show Else      = "else"
    show Error     = "error"
    show False'    = "false"
    show If        = "if"
    show In        = "in"
    show Is        = "is"
    show Let       = "let"
    show Meta      = "meta"
    show Not       = "not"
    show Otherwise = "otherwise"
    show Or        = "or"
    show Section   = "section"
    show Shared    = "shared"
    show Then      = "then"
    show True'     = "true"
    show Try       = "try"
    show Type'     = "type"
    show H_Binary       = "#binary"
    show H_Date         = "#date"
    show H_DateTime     = "#datetime"
    show H_DateTimezone = "#datetimezone"
    show H_Duration     = "#duration"
    show H_Infinity     = "#infinity"
    show H_Nan          = "#nan"
    show H_Sections     = "#sections"
    show H_Shared       = "#shared"
    show H_Table        = "#table"
    show H_Time         = "#time"


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
    deriving (Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

operators :: [Operator]
operators = [toEnum 0 .. ]

instance Show Operator where
    show Comma        = ","
    show SemiColon    = ";"
    show Equal        = "="
    show LT'          = "<"
    show LEQ          = "<="
    show GT'          = ">"
    show GEQ          = ">="
    show NEQ          = "<>"
    show Plus         = "+"
    show Minus        = "-"
    show Mult         = "*"
    show Div          = "/"
    show Ampersand    = "&"
    show LeftParen    = "("
    show RightParen   = ")"
    show LeftBracket  = "["
    show RightBracket = "]"
    show LeftCurly    = "{"
    show RightCurly   = "}"
    show At           = "@"
    show QMark        = "?"
    show Arrow        = "=>"
    show TwoDots      = ".."
    show ThreeDots    = "..."

