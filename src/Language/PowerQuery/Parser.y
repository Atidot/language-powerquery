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
    'literal'       { TLiteral _             }

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
    'nullable'     { TComment }

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
    '!'     { TComment }

%%

-- 12.2.3.1 - Expressions
expression :: { Expression Annotation }
expression
    : logical_or_expression     { Logical $1       }
    | each_expression           { Each' $1         }
    | function_expression       { Function $1      }
    | let_expression            { Let' $1          }
    | if_expression             { If' $1           }
    | error_raising_expression  { ErrorRaising $1  }
    | error_handling_expression { ErrorHandling $1 }

-- 12.2.3.2 - Logical expressions
logical_or_expression :: { LogicalOrExpression Annotation }
logical_or_expression
    : logical_and_expression                            { And'' $1 }
    | logical_and_expression 'or' logical_or_expression { Or'   $1 $2 }

logical_and_expression :: { LogicalAndExpression Annotation }
logical_and_expression
    : is_expression                              { Is'''  $1 }
    | logical_and_expression 'and' is_expression { And''' $1 }

-- 12.2.3.3 - Is expression
is_expression :: { IsExpression Annotation }
is_expression
    : as_expression                              { As'' $1 }
    | is_expression 'is' nullable_primitive_type { Is'  $1 $3 }

-- TODO: fix
nullable_primitive_type :: { PrimitiveType }
nullable_primitive_type
    : 'nullable' primitive_type { NullablePrimitiveType $2 True }

-- 12.2.3.4 -- As expression
as_expression :: { AsExpression Annotation }
as_expression
    : equality_expression { Equality $1 }
    | as_expression 'as' nullable_primitive_type { As' $1 $3 }


-- 12.2.3.5 -- Equality expression
equality_expression :: { EqualityExpression Annotation }
equality_expression
    : relational_expression                          { Relational $1 }
    | relational_expression '='  equality_expression { EqR  $1 $3    }
    | relational_expression '<>' equality_expression { NeqR $1 $3    }

-- 12.2.3.6 -- Relational expression
relational_expression :: { RelationalExpression Annotation }
relational_expression
    : additive_expression                            { Additive $1 }
    | additive_expression '<'  relational_expression { LtR  $1 $3  }
    | additive_expression '>'  relational_expression { GtR  $1 $3  }
    | additive_expression '<=' relational_expression { LeqR $1 $3  }
    | additive_expression '>=' relational_expression { GeqR $1 $3  }

-- 12.2.3.7 - Arithmetic expressions
additive_expression :: { AdditiveExpression Annotation }
additive_expression
    : multiplicative_expression                         { Multiplicative $1 }
    | multiplicative_expression '+' additive_expression { Plus' $1 $3 }
    | multiplicative_expression '-' additive_expression { Minus' $1 $3 }
    | multiplicative_expression '&' additive_expression { And' $1 $3 }

multiplicative_expression :: { MultiplicativeExpression Annotation }
multiplicative_expression
    : metadata_expression { Metadata $1 }
    | metadata_expression '*' multiplicative_expression { Mult' $1 $3 }
    | metadata_expression '/' multiplicative_expression { Div'  $1 $3 }

-- 12.2.3.8 - Metadata expression
metadata_expression :: { MetadataExpression Annotation }
metadata_expression
    : unary_expression                         { MetadataExpression $1 Nothing   (Just Annotation) }
    | unary_expression 'meta' unary_expression { MetadataExpression $1 (Just $3) (Just Annotation) }

-- 12.2.3.9 - Unary expression
unary_expression :: { UnaryExpression Annotation }
unary_expression
    : type_expression        { UnaryType  $1 }
    | '+'   unary_expression { UnaryPlus  $2 }
    | '-'   unary_expression { UnaryMinus $2 }
    | 'not' unary_expression { UnaryNot   $2 }

-- 12.2.3.10 - Primary expression
primary_expression :: { PrimaryExpression Annotation }
primary_expression
    : literal_expression         { Literal $1        }
    | list_expression            { List $1           }
    | record_expression          { Record $1         }
    | identifier_expression      { Identifier' $1    }
    | section_access_expression  { SectionAccess $1  }
    | parenthesized_expression   { Parenthesized $1  }
    | field_access_expression    { FieldAccess $1    }
    | item_access_expression     { ItemAccess $1     }
    | invoke_expression          { Invoke $1         }
    | not_implemented_expression { NotImplemented $1 }



-- 12.2.3.11 - Literal expression
literal_expression :: { LiteralExpression Annotation }
literal_expression
    : 'literal' { LiteralExpression (getLiteral $1) (Just Annotation) }

-- 12.2.3.12 - Identifier expression
identifier_expression :: { IdentifierExpression Annotation }
identifier_expression
    : identifier_reference { $1 }

identifier_reference :: { IdentifierExpression Annotation }
identifier_reference
    : exclusive_identifier_reference { $1 }
    | inclusive_identifier_reference { $1 }

exclusive_identifier_reference :: { IdentifierExpression Annotation }
exclusive_identifier_reference
    : 'identifier' { IdentifierExpression (getIdent $1) False (Just Annotation) }

inclusive_identifier_reference :: { IdentifierExpression Annotation }
inclusive_identifier_reference
    : '@' 'identifier' { IdentifierExpression (getIdent $2) True (Just Annotation) }

-- 12.2.3.13 - Section-access expression
section_access_expression :: { SectionAccessExpression Annotation }
section_access_expression
    : 'identifier' '!' 'identifier' { SectionAccessExpression (getIdent $1) (getIdent $2) (Just Annotation) }

-- 12.2.3.14 - Parenthesized expression
parenthesized_expression :: { ParenthesizedExpression Annotation }
parenthesized_expression
    : '(' expression ')' { ParenthesizedExpression $2 (Just Annotation) }

-- 12.2.3.15 - Not-implemented experssion
not_implemented_expression :: { NotImplementedExpression Annotation }
not_implemented_expression
    : '...' { NotImplementedExpression (Just Annotation) }

-- 12.2.3.16 - Invoke expression
invoke_expression :: { InvokeExpression Annotation }
invoke_expression
    : primary_expression '(' argument_list ')' { InvokeExpression $1 $3 (Just Annotation) }

argument_list :: { [Expression Annotation] }
argument_list
    : expression                   { [$1] }
    | expression ',' argument_list { $1:$3 }

-- 12.2.3.17 - List expression
list_expression :: { ListExpression Annotation }
list_expression
    : '{' item_list '}' { ListExpression $2 (Just Annotation) }

item_list :: { [Item Annotation] }
item_list
    : item               { [$1] }
    | item ',' item_list { $1:$3 }

item :: { Item Annotation }
item
    : expression                 { Item $1 Nothing   (Just Annotation) }
    | expression '..' expression { Item $1 (Just $3) (Just Annotation) }

-- 12.2.3.18 - Record expression
record_expression :: { RecordExpression Annotation }
record_expression
    : '[' field_list ']' { RecordExpression $2 (Just Annotation) }

field_list :: { [Field Annotation] }
field_list
    : field                 { [$1] }
    | field ',' field_list  { $1:$3 }

field :: { Field Annotation }
field
    : field_name '=' expression { Field $1 $3 (Just Annotation) }

field_name :: { Identifier }
field_name
    : 'identifier' { getIdent $1 }

-- 12.2.3.19 - Item access expression
item_access_expression :: { ItemAccessExpression Annotation }
item_access_expression
    : item_selection           { $1 }
    | optional_item_selection  { $1 }

item_selection :: { ItemAccessExpression Annotation }
item_selection
    : primary_expression '{' item_selector '}' { ItemAccessExpression $1 $3 False (Just Annotation) }

optional_item_selection :: { ItemAccessExpression Annotation }
optional_item_selection
    : primary_expression '{' item_selector '}' '?' { ItemAccessExpression $1 $3 True (Just Annotation) }

-- 6.4.1 - Item Access
item_selector :: { Expression Annotation }
item_selector
    : expression { $1 }

-- 12.2.3.20 - Field access expressions
field_access_expression :: { FieldAccessExpression Annotation }
field_access_expression
    : field_selection                 { $1 }
    | implicit_target_field_selection { $1 }
    | projection                      { $1 }
    | implicit_target_projection      { $1 }

field_selection :: { FieldAccessExpression Annotation }
field_selection
    : primary_expression field_selector { FieldSelection $1 $2 (Just Annotation) }

field_selector :: { FieldSelector Annotation }
field_selector
    : required_field_selector { $1 }
    | optional_field_selector { $1 }

required_field_selector :: { FieldSelector Annotation }
required_field_selector
    : '[' field_name ']' { FieldSelector $2 False (Just Annotation) }

optional_field_selector :: { FieldSelector Annotation }
optional_field_selector
    : '[' field_name ']' '?' { FieldSelector $2 True (Just Annotation) }


implicit_target_field_selection :: { FieldAccessExpression Annotation }
implicit_target_field_selection
    : field_selector { ImplicitTargetFieldSelection $1 (Just Annotation) }

projection :: { FieldAccessExpression Annotation }
projection
    : primary_expression required_projection { Projection $1 $2 False (Just Annotation) }
    | primary_expression optional_projection { Projection $1 $2 True  (Just Annotation) }

required_projection :: { [FieldSelector Annotation] }
required_projection
    : '[' required_selector_list ']' { $2 }

optional_projection :: { [FieldSelector Annotation] }
optional_projection
    : '[' required_selector_list ']' '?' { $2 }

required_selector_list :: { [FieldSelector Annotation] }
required_selector_list
    : required_field_selector                            { [$1] }
    | required_field_selector ',' required_selector_list { $1:$3 }

implicit_target_projection :: { FieldAccessExpression Annotation }
implicit_target_projection
    : 'identifier'   { ImplicitTargetProjection Nothing }


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
    : 'identifier' { getIdent $1 }

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
    : 'identifier' { getIdent $1 }

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

-- 12.2.3.26 - Error raising expression
error_raising_expression :: { ErrorRaisingExpression Annotation }
error_raising_expression
    : 'error' expression { ErrorRaisingExpression $2 (Just Annotation) }

-- 12.2.3.27 - Error handling expression
error_handling_expression :: { ErrorHandlingExpression Annotation }
error_handling_expression
    : 'try' protected_expression otherwise_clause { ErrorHandlingExpression $2 (Just $3) (Just Annotation) }

protected_expression :: { Expression Annotation }
protected_expression
    : expression { $1 }

otherwise_clause :: { Expression Annotation }
otherwise_clause
    : 'otherwise' default_expression { $2 }

default_expression :: { Expression Annotation }
default_expession
    : expression { $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse errror"

getIdent :: Token -> Identifier
getIdent (TIdentifier ident) = ident
getIdent _ = error "Not a TIdentifier"

getLiteral :: Token -> Literal
getLiteral (TLiteral literal) = literal
getLiteral _ = error "Not a TIdentifier"

}
