{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.PowerQuery.PrettyPrinter where

import "base"                    Data.Monoid ((<>))
import "text"                    Data.Text (Text, pack, intercalate)
import "language-powerquery-ast" Language.PowerQuery.AST

class PrettyPrint a where
    pprint :: a -> Text

instance (PrettyPrint a) => PrettyPrint (Maybe a) where
    pprint Nothing  = ""
    pprint (Just x) = pprint x


-- 12.1.3 Tokens
instance PrettyPrint Token where
    pprint TComment        = ""
    pprint (TLiteral l)    = pprint l
    pprint (TIdentifier i) = pprint i
    pprint (TKeyword k)    = pprint k
    pprint (TOperator o)   = pprint o
    pprint TEOF            = ""

-- 12.1.5 Literals
instance PrettyPrint Literal where
    pprint (Logical' True)  = "true"
    pprint (Logical' False) = "false"
    pprint (Integer' i)     = pack . show $ i
    pprint (Float'   f)     = pack . show $ f
    pprint (String'  t)     = t
    pprint Null             = "null"

-- 12.1.6 Identifiers
instance PrettyPrint Identifier where
    pprint (RegularIdentifier t) = t
    pprint (QuotedIdentifier  t) = t -- "#\"" <> t <> "\""

-- 12.1.7 Keywords and predefined identifiers
instance PrettyPrint Keyword where
    pprint And       = "and"
    pprint As        = "as"
    pprint Each      = "each"
    pprint Else      = "else"
    pprint Error     = "error"
    pprint False'    = "false"
    pprint If        = "if"
    pprint In        = "in"
    pprint Is        = "is"
    pprint Let       = "let"
    pprint Meta      = "meta"
    pprint Not       = "not"
    pprint Otherwise = "otherwise"
    pprint Or        = "or"
    pprint Section   = "section"
    pprint Shared    = "shared"
    pprint Then      = "then"
    pprint True'     = "true"
    pprint Try       = "try"
    pprint Type'     = "type"
    pprint H_Binary       = "#binary"
    pprint H_Date         = "#date"
    pprint H_DateTime     = "#datetime"
    pprint H_DateTimezone = "#datetimezone"
    pprint H_Duration     = "#duration"
    pprint H_Infinity     = "#infinity"
    pprint H_Nan          = "#nan"
    pprint H_Sections     = "#sections"
    pprint H_Shared       = "#shared"
    pprint H_Table        = "#table"
    pprint H_Time         = "#time"

-- 12.1.8 Operators and punctuators
instance PrettyPrint Operator where
    pprint Comma        = ","
    pprint SemiColon    = ";"
    pprint Equal        = "="
    pprint LT'          = "<"
    pprint LEQ          = "<="
    pprint GT'          = ">"
    pprint GEQ          = ">="
    pprint NEQ          = "<>"
    pprint Plus         = "+"
    pprint Minus        = "-"
    pprint Mult         = "*"
    pprint Div          = "/"
    pprint Ampersand    = "&"
    pprint LeftParen    = "("
    pprint RightParen   = ")"
    pprint LeftBracket  = "["
    pprint RightBracket = "]"
    pprint LeftCurly    = "{"
    pprint RightCurly   = "}"
    pprint At           = "@"
    pprint QMark        = "?"
    pprint Arrow        = "=>"
    pprint TwoDots      = ".."
    pprint ThreeDots    = "..."

-- 12.2.1 Documents
instance (PrettyPrint a) => PrettyPrint (Document a) where
    pprint (SectionDocument section')       = pprint section'
    pprint (ExpressionDocument expression') = pprint expression'

-- 12.2.2 Section Documents
instance (PrettyPrint a) => PrettyPrint (Section a) where
    pprint (Section' mAttrs mName members _)
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
    pprint (Record' r)  = pprint r
    pprint (List'   ls) = pprint ls
    pprint (Literal' l) = pprint l
