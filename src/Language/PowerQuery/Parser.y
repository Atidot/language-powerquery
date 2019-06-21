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
    'identifier'    { TIdentifier _          }

    'and'           { TKeyword And            }
    'as'            { TKeyword As             }
    'each'          { TKeyword Each           }
    'else'          { TKeyword Else           }
    'error'         { TKeyword Error          }
    'false'         { TKeyword False'         }
    'if'            { TKeyword If             }
    'in'            { TKeyword In             }
    'is'            { TKeyword Is             }
    'let'           { TKeyword Let            }
    'meta'          { TKeyword Meta           }
    'not'           { TKeyword Not            }
    'otherwise'     { TKeyword Otherwise      }
    'or'            { TKeyword Or             }
    'section'       { TKeyword Section        }
    'shared'        { TKeyword Shared         }
    'then'          { TKeyword Then           }
    'true'          { TKeyword True'          }
    'try'           { TKeyword Try            }
    'type'          { TKeyword Type'          }
    '#binary'       { TKeyword H_Binary       }
    '#date'         { TKeyword H_Date         }
    '#datetime'     { TKeyword H_DateTime     }
    '#datetimezone' { TKeyword H_DateTimezone }
    '#duration'     { TKeyword H_Duration     }
    '#infinity'     { TKeyword H_Infinity     }
    '#nan'          { TKeyword H_Nan          }
    '#sections'     { TKeyword H_Sections     }
    '#shared'       { TKeyword H_Shared       }
    '#table'        { TKeyword H_Table        }
    '#time'         { TKeyword H_Time         }

    ','             { TOperator Comma        }
    ';'             { TOperator SemiColon    }
    '='             { TOperator Equal        }
    '<'             { TOperator LT'          }
    '<='            { TOperator LEQ          }
    '>'             { TOperator GT'          }
    '>='            { TOperator GEQ          }
    '<>'            { TOperator NEQ          }
    '+'             { TOperator Plus         }
    '-'             { TOperator Minus        }
    '*'             { TOperator Mult         }
    '/'             { TOperator Div          }
    '&'             { TOperator Ampersand    }
    '('             { TOperator LeftParen    }
    ')'             { TOperator RightParen   }
    '['             { TOperator LeftBracket  }
    ']'             { TOperator RightBracket }
    '{'             { TOperator LeftCurly    }
    '}'             { TOperator RightCurly   }
    '@'             { TOperator At           }
    '?'             { TOperator QMark        }
    '=>'            { TOperator Arrow        }
    '..'            { TOperator TwoDots      }
    '...'           { TOperator ThreeDots    }

    'optional'     { TComment }

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
    'xxx'   { TComment }

%%

-- 12.2.3.1 - Expressions
expression :: { Expression Annotation }
expression
    : 'xxx' { Logical $ AndExpression Annotation }

-- 12.2.3.9 - Unary expression
unary_expression :: { UnaryExpression Annotation }
unary_expression
    : type_expression        { UnaryExpression Annotation }
    | '+'   unary_expression { UnaryExpression Annotation }
    | '-'   unary_expression { UnaryExpression Annotation }
    | 'not' unary_expression { UnaryExpression Annotation }

-- 12.2.3.20 -- Field access expressions


-- 12.2.3.21 - Function expression
function_expression :: { FunctionExpression Annotation }
function_expression
    : '(' parameter_list ')' return_type '=>' function_body { FunctionExpression $6 (Just $2) (Just $4) (Just Annotation) }

function_body :: { Expression Annotation }
function_body
    : expression { $1 }

parameter_list :: { [Parameter Annotation] }
parameter_list
    : fixed_parameter_list                             { $1 }
    | fixed_parameter_list ',' optional_parameter_list { $1 <> $3 }
    | optional_parameter_list                          { $1 }

fixed_parameter_list :: { [Parameter Annotation] }
fixed_parameter_list
    : parameter                           { [$1] }
    | parameter ',' fixed_parameter_list  { $1:$3 }

parameter :: { Parameter Annotation }
parameter
    : parameter_name parameter_type { Parameter $1 (Just $2) False (Just Annotation) }

parameter_name :: { Identifier }
parameter_name
    : 'identifier' { (\(TIdentifier ident) -> ident) $1 }

parameter_type :: { Type Annotation }
parameter_type
    : assertion   { $1 }

return_type :: { Type Annotation }
return_type
    : assertion { $1 }

assertion :: { Type Annotation }
assertion
    : 'as' type { $2 }

optional_parameter_list :: { [Parameter Annotation] }
optional_parameter_list
    : optional_parameter                              { [$1] }
    | optional_parameter ',' optional_parameter_list  { $1:$3 }

optional_parameter :: { Parameter Annotation }
optional_parameter
    : 'optional' parameter { $2{ _parameter_optional = True} }

-- 12.2.3.22 - Each expression
each_expression :: { EachExpression Annotation }
each_expression
    : 'each' each_expression_body { EachExpression $2 (Just Annotation) }

each_expression_body :: { Expression Annotation }
each_expression_body
    : function_body { $1 }

-- 12.2.3.23 - Let expression
let_expression :: { LetExpression Annotation }
let_expression
    : 'let' variable_list 'in' expression { LetExpression $2 $4 (Just Annotation) }

variable_list :: { [Variable Annotation] }
variable_list
    : variable                   { [$1] }
    | variable ',' variable_list { $1 : $3 }

variable :: { Variable Annotation }
variable
    : variable_name '=' expression { Variable $1 $3 (Just Annotation) }

variable_name :: { Identifier }
variable_name
    : 'identifier' { (\(TIdentifier ident) -> ident) $1 }

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

type :: { Type Annotation }
type
    : primary_type { Type $1 (Just Annotation) }

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
    : 'any'          { TAny          }
    | 'anynonnull'   { TAnyNonNull   }
    | 'binary'       { TBinary       }
    | 'date'         { TDateTime     }
    | 'datetimezone' { TDateTimezone }
    | 'duration'     { TDuration     }
    | 'function'     { TFunction     }
    | 'list'         { TList         }
    | 'logical'      { TLogical      }
    | 'none'         { TNone         }
    | 'null'         { TNull         }
    | 'number'       { TNumber       }
    | 'record'       { TRecord       }
    | 'table'        { TTable        }
    | 'text'         { TText         }
    | 'type'         { TType         }

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
