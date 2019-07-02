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
import               Language.PowerQuery.AST.Token
import               Language.PowerQuery.AST.Annotation
import               Language.PowerQuery.AST.AST

}

%name parseDocument                      document
%name parseSectionDocument               section_document
%name parseSection                       section
%name parseSectionName                   section_name
%name parseSectionMembers                section_members
%name parseSectionMember                 section_member
%name parseSectionMemberName             section_member_name
%name parseExpressionDocument            expression_document
%name parseExpression                    expression
%name parseLogicalOrExpression           logical_or_expression
%name parseLogicalAndExpression          logical_and_expression
%name parseIsExpression                  is_expression
%name parseNullalblePrimitiveType        nullable_primitive_type
%name parseAsExpression                  as_expression
%name parseEqualityExpression            equality_expression
%name parseRelationalExpression          relational_expression
%name parseAdditiveExpression            additive_expression
%name parseMultiplicativeExpression      multiplicative_expression
%name parseMetadataExpression            metadata_expression
%name parseUnaryExpression               unary_expression
%name parsePrimaryExpression             primary_expression
%name parseLiteralExpression             literal_expression
%name parseIdentifierExpression          identifier_expression
%name parseIdentifierReference           identifier_reference
%name parseExclusiveIdentifierReference  exclusive_identifier_reference
%name parseInclusiveIdentifierReference  inclusive_identifier_reference
%name parseSectionAccessExpression       section_access_expression
%name parseParenthesizedExpression       parenthesized_expression
%name parseNotImplementedExpression      not_implemented_expression
%name parseInvokeExpression              invoke_expression
%name parseArgumentList                  argument_list
%name parseListExpression                list_expression
%name parseItemList                      item_list
%name parseItem                          item
%name parseRecordExpression              record_expression
%name parseFieldList                     field_list
%name parseField                         field
%name parseFieldName                     field_name
%name parseItemAccessExpression          item_access_expression
%name parseItemSelection                 item_selection
%name parseOptionalItemSelection         optional_item_selection
%name parseItemSelector                  item_selector
%name parseFieldAccessExpression         field_access_expression
%name parseFieldSelection                field_selection
%name parseFieldSelector                 field_selector
%name parseRequiredFieldSelector         required_field_selector
%name parseOptionalFieldSelector         optional_field_selector
%name parseImplicitTargetFieldSelection  implicit_target_field_selection
%name parseProjection                    projection
%name parseRequiredProjection            required_projection
%name parseOptionalProjection            optional_projection
%name parseRequiredSelectorList          required_selector_list
%name parseImplicitTargetProjection      implicit_target_projection
%name parseFunctionExpression            function_expression
%name parseFunctionBody                  function_body
%name parseParameterList                 parameter_list
%name parseFixedParameterList            fixed_parameter_list
%name parseParameter                     parameter
%name parseParameterName                 parameter_name
%name parseParameterType                 parameter_type
%name parseReturnType                    return_type
%name parseAssertion                     assertion
%name parseOptionalParameterList         optional_parameter_list
%name parseOptionalParameter             optional_parameter
%name parseEachExpression                each_expression
%name parseEachExpressionBody            each_expression_body
%name parseLetExpression                 let_expression
%name parseVariableList                  variable_list
%name parseVariable                      variable
%name parseVariableName                  variable_name
%name parseIfExpression                  if_expression
%name parseIfCondition                   if_condition
%name parseTrueExpression                true_expression
%name parseFalseExpression               false_expression
%name parseTypeExpression                type_expression
%name parseType                          type
%name parsePrimaryType                   primary_type
%name parsePrimitiveType                 primitive_type
%name parseRecordType                    record_type
%name parseListType                      list_type
%name parseFunctionType                  function_type
%name parseTableType                     table_type
%name parseNullableType                  nullable_type
%name parseErrorRaisingExpression        error_raising_expression
%name parseErrorHandlingExpression       error_handling_expression
%name parseProtectedExpression           protected_expression
%name parseOtherwiseClause               otherwise_clause
%name parseDefaultExpression             default_expression
%name parseLiteralAttributes             literal_attributes
%name parseRecordLiteral                 record_literal
%name parseLiteralFieldList              literal_field_list
%name parseLiteralField                  literal_field
%name parseListLiteral                   list_literal
%name parseLiteramItemList               literal_item_list
%name parseAnyLiteral                    any_literal
%name parseLogicalLiteral                logical_literal

%tokentype { Token }
%error { parseError }

%token
    'and'           { KeywordT AndK            }
    'as'            { KeywordT AsK             }
    'each'          { KeywordT EachK           }
    'else'          { KeywordT ElseK           }
    'error'         { KeywordT ErrorK          }
    'false'         { KeywordT FalseK          }
    'if'            { KeywordT IfK             }
    'in'            { KeywordT InK             }
    'is'            { KeywordT IsK             }
    'let'           { KeywordT LetK            }
    'meta'          { KeywordT MetaK           }
    'not'           { KeywordT NotK            }
    'otherwise'     { KeywordT OtherwiseK      }
    'or'            { KeywordT OrK             }
    'section'       { KeywordT SectionK        }
    'shared'        { KeywordT SharedK         }
    'then'          { KeywordT ThenK           }
    'true'          { KeywordT TrueK           }
    'try'           { KeywordT TryK            }
    'type'          { KeywordT TypeK           }
    '#binary'       { KeywordT H_BinaryK       }
    '#date'         { KeywordT H_DateK         }
    '#datetime'     { KeywordT H_DateTimeK     }
    '#datetimezone' { KeywordT H_DateTimezoneK }
    '#duration'     { KeywordT H_DurationK     }
    '#infinity'     { KeywordT H_InfinityK     }
    '#nan'          { KeywordT H_NanK          }
    '#sections'     { KeywordT H_SectionsK     }
    '#shared'       { KeywordT H_SharedK       }
    '#table'        { KeywordT H_TableK        }
    '#time'         { KeywordT H_TimeK         }

    ','             { OperatorT CommaO        }
    ';'             { OperatorT SemiColonO    }
    '='             { OperatorT EqualO        }
    '<'             { OperatorT LT_O          }
    '<='            { OperatorT LEQ_O         }
    '>'             { OperatorT GT_O          }
    '>='            { OperatorT GEQ_O         }
    '<>'            { OperatorT NEQ_O         }
    '+'             { OperatorT PlusO         }
    '-'             { OperatorT MinusO        }
    '*'             { OperatorT MultO         }
    '/'             { OperatorT DivO          }
    '&'             { OperatorT AmpersandO    }
    '('             { OperatorT LeftParenO    }
    ')'             { OperatorT RightParenO   }
    '['             { OperatorT LeftBracketO  }
    ']'             { OperatorT RightBracketO }
    '{'             { OperatorT LeftCurlyO    }
    '}'             { OperatorT RightCurlyO   }
    '@'             { OperatorT AtO           }
    '?'             { OperatorT QMarkO        }
    '=>'            { OperatorT ArrowO        }
    '..'            { OperatorT TwoDotsO      }
    '...'           { OperatorT ThreeDotsO    }

    'optional'     { CommentT }
    'nullL'        { LiteralT NullL }
    'numberL'       { LiteralT (NumberL _) }
    'textL'         { LiteralT (StringL _) }

    'nullable'     { CommentT }

    'any'          { IdentifierT (RegularI "any") }
    'anynonnull'   { IdentifierT (RegularI "anynonnull") }
    'binary'       { IdentifierT (RegularI "binary") }
    'date'         { IdentifierT (RegularI "date") }
    'datetimezone' { IdentifierT (RegularI "datetimezone") }
    'duration'     { IdentifierT (RegularI "duration") }
    'function'     { IdentifierT (RegularI "function") }
    'list'         { IdentifierT (RegularI "list") }
    'logical'      { IdentifierT (RegularI "logical") }
    'none'         { IdentifierT (RegularI "none") }
    'null'         { IdentifierT (RegularI "null") }
    'number'       { IdentifierT (RegularI "number") }
    'record'       { IdentifierT (RegularI "record") }
    'table'        { IdentifierT (RegularI "table") }
    'text'         { IdentifierT (RegularI "text") }

    'xxx'          { CommentT }
    '!'            { CommentT }

    'identifier'    { IdentifierT _          }
    'literal'       { LiteralT _             }


%%

-- 12.2.1 - Documents
document :: { Document Annotation }
document
    : section_document    { $1 }
    | expression_document { $1 }

-- 12.2.2 - Section Documents
section_document :: { Document Annotation }
section_document
    : section { SectionDocument $1 }

section :: { Section Annotation }
section
    : literal_attributes__opt 'section' section_name__opt ';' section_members__opt { Section $1 $3 $5 (Just Annotation) }

literal_attributes__opt :: { Maybe (RecordLiteral annotation) }
literal_attributes__opt
    : literal_attributes { Just $1 }
    | {- empty -}        { Nothing}

section_name__opt :: { Maybe Identifier }
section_name__opt
    : section_name   { Just $1 }
    | {- empty -}    { Nothing }

section_members__opt :: { Maybe [SectionMember annotation] }
section_members__opt
    : section_members    { Just $1 }
    | {- empty -}        { Nothing }

section_name :: { Identifier }
section_name
    : 'identifier' { getIdent $1 }

section_members :: { [SectionMember Annotation] }
section_members
    : section_member                 { [$1] }
    | section_member section_members { $1:$2 }

section_member :: { SectionMember Annotation }
section_member
    : literal_attributes__opt shared__opt section_member_name '=' expression ';' { SectionMember $1 $2 $3 $5 (Just Annotation) }

shared__opt :: { Bool }
shared__opt
    : 'shared'    { True }
    | {- empty -} { False }

section_member_name :: { Identifier }
section_member_name
    : 'identifier' { getIdent $1 }

-- 12.2.3.1 - Expressions
expression_document :: { Document Annotation }
expression_document
    : expression { ExpressionDocument $1 }

expression :: { Expression Annotation }
expression
    : logical_or_expression     { LogicalE $1       }
    | each_expression           { EachE $1          }
    | function_expression       { FunctionE $1      }
    | let_expression            { LetE $1           }
    | if_expression             { IfE $1            }
    | error_raising_expression  { ErrorRaisingE $1  }
    | error_handling_expression { ErrorHandlingE $1 }

-- 12.2.3.2 - Logical expressions
logical_or_expression :: { LogicalOrExpression Annotation }
logical_or_expression
    : logical_and_expression                            { And_OE $1 }
    | logical_and_expression 'or' logical_or_expression { Or_OE   $1 $3 }

logical_and_expression :: { LogicalAndExpression Annotation }
logical_and_expression
    : is_expression                              { Is_LAE  $1 }
    | logical_and_expression 'and' is_expression { And_LAE $1 $3 }

-- 12.2.3.3 - Is expression
is_expression :: { IsExpression Annotation }
is_expression
    : as_expression                              { As_IE $1 }
    | is_expression 'is' nullable_primitive_type { Is_IE  $1 $3 }

nullable_primitive_type :: { NullablePrimitiveType }
nullable_primitive_type
    : nullable__opt primitive_type { NullablePrimitiveType $2 $1 }

nullable__opt :: { Bool }
nullable__opt
    : 'nullable'  { True }
    | {- empty -} { False }

-- 12.2.3.4 -- As expression
as_expression :: { AsExpression Annotation }
as_expression
    : equality_expression { EqualityAE $1 }
    | as_expression 'as' nullable_primitive_type { As_AE $1 $3 }


-- 12.2.3.5 -- Equality expression
equality_expression :: { EqualityExpression Annotation }
equality_expression
    : relational_expression                          { RelationalEE $1 }
    | relational_expression '='  equality_expression { Eq_EE  $1 $3    }
    | relational_expression '<>' equality_expression { Neq_EE $1 $3    }

-- 12.2.3.6 -- Relational expression
relational_expression :: { RelationalExpression Annotation }
relational_expression
    : additive_expression                            { AdditiveRE $1 }
    | additive_expression '<'  relational_expression { Lt_RE  $1 $3  }
    | additive_expression '>'  relational_expression { Gt_RE $1 $3   }
    | additive_expression '<=' relational_expression { Leq_RE $1 $3  }
    | additive_expression '>=' relational_expression { Geq_RE $1 $3  }

-- 12.2.3.7 - Arithmetic expressions
additive_expression :: { AdditiveExpression Annotation }
additive_expression
    : multiplicative_expression                         { MultiplicativeAE $1 }
    | multiplicative_expression '+' additive_expression { PlusAE $1 $3 }
    | multiplicative_expression '-' additive_expression { MinusAE $1 $3 }
    | multiplicative_expression '&' additive_expression { And_AE $1 $3 }

multiplicative_expression :: { MultiplicativeExpression Annotation }
multiplicative_expression
    : metadata_expression { MetadataME $1 }
    | metadata_expression '*' multiplicative_expression { MultME $1 $3 }
    | metadata_expression '/' multiplicative_expression { DivME  $1 $3 }

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
    : literal_expression         { LiteralPE $1        }
    | list_expression            { ListPE $1           }
    | record_expression          { RecordPE $1         }
    | identifier_expression      { IdentifierPE $1     }
    | section_access_expression  { SectionAccessPE $1  }
    | parenthesized_expression   { ParenthesizedPE $1  }
    | field_access_expression    { FieldAccessPE $1    }
    | item_access_expression     { ItemAccessPE $1     }
    | invoke_expression          { InvokePE $1         }
    | not_implemented_expression { NotImplementedPE $1 }



-- 12.2.3.11 - Literal expression
literal_expression :: { LiteralExpression Annotation }
literal_expression
    : 'nullL'  { LiteralExpression (getLiteral $1)  (Just Annotation) }
    | 'textL'   { LiteralExpression (getLiteral $1)  (Just Annotation) }
    | 'numberL' { LiteralExpression (getLiteral $1)  (Just Annotation) }
    | 'true'   { LiteralExpression (LogicalL True)  (Just Annotation) }
    | 'false'  { LiteralExpression (LogicalL False) (Just Annotation) }

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
    : primary_expression '(' argument_list__opt ')' { InvokeExpression $1 $3 (Just Annotation) }

argument_list__opt :: { Maybe [Expression Annotation] }
argument_list__opt
    : argument_list { Just $1 }
    | {- empty -}   { Nothing }

argument_list :: { [Expression Annotation] }
argument_list
    : expression                   { [$1] }
    | expression ',' argument_list { $1:$3 }

-- 12.2.3.17 - List expression
list_expression :: { ListExpression Annotation }
list_expression
    : '{' item_list__opt '}' { ListExpression $2 (Just Annotation) }

item_list__opt :: { Maybe [Item Annotation] }
item_list__opt
    : item_list   { Just $1 }
    | {- empty -} { Nothing }

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
    : 'identifier'   { ImplicitTargetProjection (getIdent $1) (Just Annotation) }


-- 12.2.3.21 - Function expression
function_expression :: { FunctionExpression Annotation }
function_expression
    : '(' parameter_list__opt ')' return_type__opt '=>' function_body { FunctionExpression $6 $2 $4 (Just Annotation) }

parameter_list__opt :: { Maybe [Parameter Annotation] }
parameter_list__opt
    : parameter_list  { Just $1 }
    | {- empty -}     { Nothing }


return_type__opt :: { Maybe (Type Annotation) }
return_type__opt
    : return_type { Just $1 }
    | {- empty -} { Nothing }

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
    : parameter_name parameter_type__opt { Parameter $1 $2 False (Just Annotation) }

parameter_type__opt :: { Maybe (Type Annotation) }
parameter_type__opt
    : parameter_type { Just $1 }
    | {- empty -}    { Nothing }

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
    : primary_expression  { Primary_TE      $1 }
    | 'type' primary_type { PrimaryType_TE  $2 }

type :: { Type Annotation }
type
    : parenthesized_expression { ParenthesizedT $1 }
    | primary_type             { TypeT $1 (Just Annotation) }

primary_type :: { PrimaryType Annotation }
primary_type
    : primitive_type  { PrimitiveType' $1 }
    | record_type     { $1 }
    | list_type       { $1 }
    | function_type   { $1 }
    | table_type      { $1 }
    | nullable_type   { $1 }

primitive_type :: { PrimitiveType }
primitive_type
    : 'any'          { TypeAny          }
    | 'anynonnull'   { TypeAnyNonNull   }
    | 'binary'       { TypeBinary       }
    | 'date'         { TypeDateTime     }
    | 'datetimezone' { TypeDateTimezone }
    | 'duration'     { TypeDuration     }
    | 'function'     { TypeFunction     }
    | 'list'         { TypeList         }
    | 'logical'      { TypeLogical      }
    | 'none'         { TypeNone         }
    | 'null'         { TypeNull         }
    | 'number'       { TypeNumber       }
    | 'record'       { TypeRecord       }
    | 'table'        { TypeTable        }
    | 'text'         { TypeText         }
    | 'type'         { TypeType         }

record_type :: { PrimaryType Annotation }
record_type
    : '[' '...' ']'                               { RecordType [] (Just Annotation) }
    | '[' field_specification_list ']'            { RecordType $2 (Just Annotation) }
    | '[' field_specification_list ',' '...' ']'  { RecordType $2 (Just Annotation) }

field_specification_list :: { [FieldSpecification Annotation] }
field_specification_list
    : field_specification                              { [$1] }
    | field_specification ',' field_specification_list { $1:$3 }

field_specification :: { FieldSpecification Annotation }
field_specification
    : optional__opt 'identifier' field_type_specification__opt { FieldSpecification (getIdent $2) $3 $1 (Just Annotation) }

optional__opt :: { Bool }
optional__opt
    : 'optional'   { True }
    | {- empty -}  { False }

field_type_specification__opt :: { Maybe (Type Annotation) }
field_type_specification__opt
    : field_type_specification { Just $1 }
    | {- empty -}              { Nothing }

field_type_specification :: { Type Annotation }
field_type_specification
    : '=' field_type { $2 }

field_type :: { Type Annotation }
field_type
    : type { $1 }

list_type :: { PrimaryType Annotation }
list_type
    : '{' item_type '}'  { ListType $2 (Just Annotation) }

item_type :: { Type Annotation }
item_type
    : type { $1 }

function_type :: { PrimaryType Annotation }
function_type
    : 'function' '(' parameter_specification_list__opt ')' return_type { FunctionType $3 $5 (Just Annotation) }

parameter_specification_list__opt :: { Maybe [ParameterSpecification Annotation] }
parameter_specification_list__opt
    : parameter_specification_list { Just $1 }
    | {- empty -}                  { Nothing }

parameter_specification_list :: { [ParameterSpecification Annotation] }
parameter_specification_list
    : required_parameter_specification_list                                           { $1 }
    | required_parameter_specification_list ',' optional_parameter_specification_list { $1 <> $3 }
    | optional_parameter_specification_list                                           { $1 }

required_parameter_specification_list :: { [ParameterSpecification Annotation] }
required_parameter_specification_list
    : required_parameter_specification                                          { [$1] }
    | required_parameter_specification ',' required_parameter_specification_list { $1:$3 }

required_parameter_specification :: { ParameterSpecification Annotation }
required_parameter_specification
    : parameter_specification { $1 }

optional_parameter_specification_list :: { [ParameterSpecification Annotation] }
optional_parameter_specification_list
    : optional_parameter_specification                                           { [$1] }
    | optional_parameter_specification ',' optional_parameter_specification_list { $1:$3 }

optional_parameter_specification :: { ParameterSpecification Annotation }
optional_parameter_specification
    : 'optional' parameter_specification { $2 { _parameterSpecification_optional = True} }

parameter_specification :: { ParameterSpecification Annotation }
parameter_specification
    : parameter_name parameter_type { ParameterSpecification $1 (Just $2) False (Just Annotation) }

table_type :: { PrimaryType Annotation }
table_type
    : 'table' row_type { TableType $2 (Just Annotation) }

row_type :: { [FieldSpecification Annotation] }
row_type
    : '[' field_specification_list ']' { $2 }

nullable_type :: { PrimaryType Annotation }
nullable_type
    : 'nullable' type { NullableType $2 (Just Annotation) }

-- 12.2.3.26 - Error raising expression
error_raising_expression :: { ErrorRaisingExpression Annotation }
error_raising_expression
    : 'error' expression { ErrorRaisingExpression $2 (Just Annotation) }

-- 12.2.3.27 - Error handling expression
error_handling_expression :: { ErrorHandlingExpression Annotation }
error_handling_expression
    : 'try' protected_expression otherwise_clause__opt { ErrorHandlingExpression $2 $3 (Just Annotation) }

protected_expression :: { Expression Annotation }
protected_expression
    : expression { $1 }

otherwise_clause__opt :: { Maybe (Expression Annotation) }
otherwise_clause__opt
    : otherwise_clause { Just $1 }
    | {- empty -}      { Nothing }

otherwise_clause :: { Expression Annotation }
otherwise_clause
    : 'otherwise' default_expression { $2 }

default_expression :: { Expression Annotation }
default_expression
    : expression { $1 }

-- 12.2.4 - Literal Attributes
literal_attributes :: { RecordLiteral Annotation }
literal_attributes
    : record_literal { $1 }

record_literal :: { RecordLiteral Annotation }
record_literal
    : '[' literal_field_list__opt ']' { RecordLiteral' $2 }

literal_field_list__opt :: { Maybe [LiteralField Annotation] }
literal_field_list__opt
    : literal_field_list { Just $1 }
    | {- empty -}        { Nothing }

literal_field_list :: { [LiteralField Annotation] }
literal_field_list
    : literal_field                        { [$1] }
    | literal_field ',' literal_field_list { $1:$3 }

literal_field :: { LiteralField Annotation }
literal_field
    : field_name '=' any_literal { LiteralField $1 $3 } 

list_literal :: { ListLiteral Annotation }
list_literal
    : '{' literal_item_list__opt '}' { ListLiteral' $2 }

literal_item_list__opt :: { Maybe [AnyLiteral Annotation] }
literal_item_list__opt
    : literal_item_list { Just $1 }
    | {- empty -}       { Nothing }

literal_item_list :: { [AnyLiteral Annotation ] }
literal_item_list
    : any_literal ',' literal_item_list { $1:$3 }

any_literal :: { AnyLiteral Annotation }
any_literal
    : record_literal  { Record_AL $1  }
    | list_literal    { List_AL $1    }
    | logical_literal { Literal_AL $1 }
    | 'textL'          { Literal_AL $ getLiteral $1 }
    | 'numberL'        { Literal_AL $ getLiteral $1 }
    | 'nullL'         { Literal_AL $ getLiteral $1 }

logical_literal :: { Literal }
logical_literal
    : 'true'  { LogicalL True }
    | 'false' { LogicalL False }

{
lexwrap = (alexMonadScan >>=)

parseError :: [Token] -> a
parseError ts = error $ "Parse error " <> show ts

getIdent :: Token -> Identifier
getIdent (IdentifierT ident) = ident
getIdent _ = error "Not a TIdentifier"

getLiteral :: Token -> Literal
getLiteral (LiteralT literal) = literal
getLiteral _ = error "Not a TIdentifier"

}
