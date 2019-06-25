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
    = WhitespaceT
    | CommentT
    | LiteralT    Literal
    | IdentifierT Identifier
    | KeywordT    Keyword
    | OperatorT   Operator
    | EOF_T
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.1.5 Literals
data Literal
    = LogicalL Bool
    | IntegerL Integer
    | FloatL   Float
    | StringL  Text
    | NullL
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.1.6 Identifiers
data Identifier
    = RegularI Text
    | QuotedI  Text
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.1.7 Keywords and predefined identifiers
data Keyword
    = AndK
    | AsK
    | EachK
    | ElseK
    | ErrorK
    | FalseK
    | IfK
    | InK
    | IsK
    | LetK
    | MetaK
    | NotK
    | OtherwiseK
    | OrK
    | SectionK
    | SharedK
    | ThenK
    | TrueK
    | TryK
    | TypeK
    | H_BinaryK
    | H_DateK
    | H_DateTimeK
    | H_DateTimezoneK
    | H_DurationK
    | H_InfinityK
    | H_NanK
    | H_SectionsK
    | H_SharedK
    | H_TableK
    | H_TimeK
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

keywords :: [Keyword]
keywords = [toEnum 0 .. ]


-- 12.1.8 Operators and punctuators
data Operator
    = CommaO
    | SemiColonO
    | EqualO
    | LT_O
    | LEQ_O
    | GT_O
    | GEQ_O
    | NEQ_O
    | PlusO
    | MinusO
    | MultO
    | DivO
    | AmpersandO
    | LeftParenO
    | RightParenO
    | LeftBracketO
    | RightBracketO
    | LeftCurlyO
    | RightCurlyO
    | AtO
    | QMarkO
    | ArrowO
    | TwoDotsO
    | ThreeDotsO
    deriving (Show, Read, Eq, Ord, Enum, Bounded, Data, Typeable, Generic)

operators :: [Operator]
operators = [toEnum 0 .. ]
