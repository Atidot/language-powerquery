{
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.PowerQuery.Parser where

import "base"        Data.Typeable (Typeable)
import "base"        Data.Data (Data)
import "base"        Control.Monad.IO.Class (liftIO)
import "text"        Data.Text (Text, pack)
import               Language.PowerQuery.Lexer
import               Language.PowerQuery.Token
import               Language.PowerQuery.Annotation
import               Language.PowerQuery.AST

}

%name powerquery
%tokentype { Token }
%error { parseError }

%token
    '+'     { TOperator Plus }
    '-'     { TOperator Minus }
    'if'    { TKeyword If }
    'then'  { TKeyword Then }
    'else'  { TKeyword Else }
    'any'          { TComment }
    'anynonnull'   { TComment }
    'binary'       { TComment }
    'date'         { TComment }
    'datetimezone' { TComment }
    'duration'     { TComment }
    'function'     { TComment }
    'list'         { TComment }
    'logical'      { TComment }
    'none'         { TComment }
    'null'         { TComment }
    'number'       { TComment }
    'record'       { TComment }
    'table'        { TComment }
    'text'         { TComment }
    'not'   { TKeyword Not }
    'type'  { TKeyword Type' }
    'xxx'   { TComment }

%%

-- 12.2.3.1 - Expressions
expression :: { Expression Annotation }
expression
    : 'xxx' { Logical AndExpression }

-- 12.2.3.9 - Unary expression
unary_expression :: { UnaryExpression Annotation }
unary_expression
    : type_expression        { UnaryExpression Annotation }
    | '+'   unary_expression { UnaryExpression Annotation }
    | '-'   unary_expression { UnaryExpression Annotation }
    | 'not' unary_expression { UnaryExpression Annotation }

-- 12.2.3.23 - Let expression
let_expression

-- 12.2.3.24 - If expression
if_expression :: { IfExpression Annotation }
if_expression
    : 'if' if_condition 'then' true_expression 'else' false_expression { IfExpression $2 $4 $6 (Just Annotation) }

if_condition :: { Expression Annotation }
if_condition
    : expression { $1 }

true_expression :: { Expression Annotation }
true_expression
    : expression { $1 }

false_expression :: { Expression Annotation }
false_expression
    : expression { $1 }


-- 12.2.3.25 -- Type expression
type_expression :: { TypeExpression Annotation }
type_expression
    : 'type' primary_type { PrimaryType' $2 }

primary_type :: { PrimaryType Annotation }
primary_type
    : primitive_type  { Primitive $1 }
    | record_type     { $1 }
    | list_type       { $1 }
    | function_type   { $1 }
    | table_type      { $1 }
    | nullable_type   { $1 }

primitive_type :: { PrimitiveType }
primitive_type
    : 'any'          { TAny }
    | 'anynonnull'   { TAnyNonNull }
    | 'binary'       { TBinary }
    | 'date'         { TDateTime }
    | 'datetimezone' { TDateTimezone }
    | 'duration'     { TDuration }
    | 'function'     { TFunction }
    | 'list'         { TList }
    | 'logical'      { TLogical }
    | 'none'         { TNone }
    | 'null'         { TNull }
    | 'number'       { TNumber }
    | 'record'       { TRecord }
    | 'table'        { TTable }
    | 'text'         { TText }
    | 'type'         { TType }

record_type :: { PrimaryType Annotation }
record_type
    : 'xxx'    { RecordType [] Nothing }

list_type :: { PrimaryType Annotation }
list_type
    : 'xxx'  { ListType undefined Nothing }

function_type :: { PrimaryType Annotation }
function_type
    : 'xxx'  { FunctionType undefined TAny Nothing }

table_type :: { PrimaryType Annotation }
table_type
    : 'xxx'  { TableType [] Nothing }

nullable_type :: { PrimaryType Annotation }
nullable_type
    : 'xxx'  { NullableType undefined Nothing }

{
parseError :: [Token] -> a
parseError _ = error "Parse errror"

}
