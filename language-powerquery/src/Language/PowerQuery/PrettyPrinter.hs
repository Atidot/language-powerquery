{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.PowerQuery.PrettyPrinter where

import "base"                    Data.Monoid ((<>))
import "text"                    Data.Text (Text, pack, intercalate)
import "language-powerquery-ast" Language.PowerQuery.AST

class PrettyPrint a where
    pprint :: a -> Text

instance PrettyPrint Annotation

instance (PrettyPrint a) => PrettyPrint (Maybe a) where
    pprint Nothing  = ""
    pprint (Just x) = pprint x


-- 12.1.3 Tokens
instance PrettyPrint Token where
    pprint CommentT        = ""
    pprint (LiteralT l)    = pprint l
    pprint (IdentifierT i) = pprint i
    pprint (KeywordT k)    = pprint k
    pprint (OperatorT o)   = pprint o
    pprint EOF_T           = ""

-- 12.1.5 Literals
instance PrettyPrint Literal where
    pprint (LogicalL True)  = "true"
    pprint (LogicalL False) = "false"
    pprint (IntegerL i)     = pack . show $ i
    pprint (FloatL   f)     = pack . show $ f
    pprint (StringL  t)     = t
    pprint NullL            = "null"

-- 12.1.6 Identifiers
instance PrettyPrint Identifier where
    pprint (RegularI t) = t
    pprint (QuotedI  t) = t -- "#\"" <> t <> "\""

-- 12.1.7 Keywords and predefined identifiers
instance PrettyPrint Keyword where
    pprint AndK       = "and"
    pprint AsK        = "as"
    pprint EachK      = "each"
    pprint ElseK      = "else"
    pprint ErrorK     = "error"
    pprint FalseK     = "false"
    pprint IfK        = "if"
    pprint InK        = "in"
    pprint IsK        = "is"
    pprint LetK       = "let"
    pprint MetaK      = "meta"
    pprint NotK       = "not"
    pprint OtherwiseK = "otherwise"
    pprint OrK        = "or"
    pprint SectionK   = "section"
    pprint SharedK    = "shared"
    pprint ThenK      = "then"
    pprint TrueK      = "true"
    pprint TryK       = "try"
    pprint TypeK      = "type"
    pprint H_BinaryK       = "#binary"
    pprint H_DateK         = "#date"
    pprint H_DateTimeK     = "#datetime"
    pprint H_DateTimezoneK = "#datetimezone"
    pprint H_DurationK     = "#duration"
    pprint H_InfinityK     = "#infinity"
    pprint H_NanK          = "#nan"
    pprint H_SectionsK     = "#sections"
    pprint H_SharedK       = "#shared"
    pprint H_TableK        = "#table"
    pprint H_TimeK         = "#time"

-- 12.1.8 Operators and punctuators
instance PrettyPrint Operator where
    pprint CommaO        = ","
    pprint SemiColonO    = ";"
    pprint EqualO        = "="
    pprint LT_O          = "<"
    pprint LEQ_O         = "<="
    pprint GT_O          = ">"
    pprint GEQ_O         = ">="
    pprint NEQ_O         = "<>"
    pprint PlusO         = "+"
    pprint MinusO        = "-"
    pprint MultO         = "*"
    pprint DivO          = "/"
    pprint AmpersandO    = "&"
    pprint LeftParenO    = "("
    pprint RightParenO   = ")"
    pprint LeftBracketO  = "["
    pprint RightBracketO = "]"
    pprint LeftCurlyO    = "{"
    pprint RightCurlyO   = "}"
    pprint AtO           = "@"
    pprint QMarkO        = "?"
    pprint ArrowO        = "=>"
    pprint TwoDotsO      = ".."
    pprint ThreeDotsO    = "..."

-- 12.2.1 Documents
instance (PrettyPrint a) => PrettyPrint (Document a) where
    pprint (SectionDocument section')       = pprint section'
    pprint (ExpressionDocument expression') = pprint expression'

-- 12.2.2 Section Documents
instance (PrettyPrint a) => PrettyPrint (Section a) where
    pprint (Section mAttrs mName members _)
        = pprint mAttrs <> " section " <> pprint mName <> " ; " <> members'
        where
            members' = mconcat . map pprint $ members

instance (PrettyPrint a) => PrettyPrint (SectionMember a) where
    pprint (SectionMember mAttrs shared name expression _)
        = pprint mAttrs <> " " <> isShared shared <> " " <> pprint name <> " = " <> pprint expression <> " ; "
        where
            isShared True  = "shared"
            isShared False = ""

-- 12.2.3.1 Expressions
instance (PrettyPrint a) => PrettyPrint (Expression a) where
    pprint _ = undefined

-- 12.2.3.10 Primary expression
instance (PrettyPrint a) => PrettyPrint (PrimaryExpression a) where
    pprint _ = undefined

-- 12.2.3.14 Parenthesized expression
instance (PrettyPrint a) => PrettyPrint (ParenthesizedExpression a) where
    pprint _ = undefined

-- 12.2.3.22 Each expression
instance (PrettyPrint a) => PrettyPrint (EachExpression a) where
    pprint (EachExpression body _) = "each " <> pprint body

-- 12.2.3.23 Let expression
instance (PrettyPrint a) => PrettyPrint (LetExpression a) where
    pprint (LetExpression variables expression _)
        = "let " <> variables' <> " in " <> pprint expression
        where
            variables' = intercalate "," . map pprint $ variables

instance (PrettyPrint a) => PrettyPrint (Variable a) where
    pprint (Variable name expression _)
        = pprint name <> " = " <> pprint expression

-- 12.2.3.24 If Expression
instance (PrettyPrint a) => PrettyPrint (IfExpression a) where
    pprint (IfExpression condition true' false' _)
        = "if " <> pprint condition <> " then " <> pprint true' <> " else " <> pprint false'

-- 12.3.2.25 Type Expression
instance (PrettyPrint a) => PrettyPrint (TypeExpression a) where
    pprint (Primary_TE pexpression) = pprint pexpression
    pprint (PrimaryType_TE ptype)  = pprint ptype

instance (PrettyPrint a) => PrettyPrint (Type a) where
    pprint (ParenthesizedT parenExpression) = pprint parenExpression
    pprint (TypeT primary _)
        = "type " <> pprint primary

instance (PrettyPrint a) => PrettyPrint (PrimaryType a) where
    pprint (PrimitiveType ptype)                   = pprint ptype
    pprint (RecordType fieldSpecs _)               = undefined
    pprint (ListType itemType _)                   = undefined
    pprint (FunctionType mParamSpecs returnType _) = undefined
    pprint (TableType rowTypes _)                  = undefined
    pprint (NullableType type' _)                  = undefined

instance (PrettyPrint a) => PrettyPrint (FieldSpecification a) where
    pprint (FieldSpecification identifier mFieldType optional _)
        = isOptional optional <> pprint identifier <> " " <> pprint mFieldType
        where
            isOptional True  = "optional "
            isOptional False = ""

instance (PrettyPrint a) => PrettyPrint (ParameterSpecification a) where
    pprint (ParameterSpecification name mType optional _)
        = isOptional optional <> pprint name <> " " <> pprint mType
        where
            isOptional True  = "optional "
            isOptional False = ""

instance PrettyPrint PrimitiveType where
    pprint TypeAny          = "any"
    pprint TypeAnyNonNull   = "anynonnull"
    pprint TypeBinary       = "binary"
    pprint TypeDate         = "date"
    pprint TypeDateTime     = "datetime"
    pprint TypeDateTimezone = "datetimezone"
    pprint TypeDuration     = "duration"
    pprint TypeFunction     = "function"
    pprint TypeList         = "list"
    pprint TypeLogical      = "logical"
    pprint TypeNone         = "none"
    pprint TypeNull         = "null"
    pprint TypeNumber       = "number"
    pprint TypeRecord       = "record"
    pprint TypeTable        = "table"
    pprint TypeText         = "text"
    pprint TypeType         = "type"

-- 12.2.3.26 Error raising expression
instance (PrettyPrint a) => PrettyPrint (ErrorRaisingExpression a) where
    pprint (ErrorRaisingExpression error _)
        = "error " <> pprint error

-- 12.2.3.27 Error handling expression
instance (PrettyPrint a) => PrettyPrint (ErrorHandlingExpression a) where
    pprint (ErrorHandlingExpression protected otherwise _)
        = "try " <> pprint protected <> " " <> printOtherwise otherwise
        where
            printOtherwise Nothing           = ""
            printOtherwise (Just expression) = "otherwise " <> pprint expression

-- 12.2.4 Literal Attributes
instance (PrettyPrint a) => PrettyPrint (RecordLiteral a) where
    pprint (RecordLiteral fields)
        = "[" <> fields' <> "]"
        where
            fields' = intercalate "," . map pprint $ fields

instance (PrettyPrint a) => PrettyPrint (LiteralField a) where
    pprint (LiteralField name literal)
        = pprint name <> " = " <> pprint literal

instance (PrettyPrint a) => PrettyPrint (ListLiteral a) where
    pprint (ListLiteral items)
        = "{" <> items' <> "}"
        where
            items' = intercalate "," . map pprint $ items

instance (PrettyPrint a) => PrettyPrint (AnyLiteral a) where
    pprint (Record_AL r)  = pprint r
    pprint (List_AL   ls) = pprint ls
    pprint (Literal_AL l) = pprint l
