{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}

module Language.PowerQuery.AST.AST where

import "base" GHC.Generics  (Generic)
import "base" Data.Typeable (Typeable)
import "base" Data.Data     (Data)
import        Language.PowerQuery.AST.Token


-- 12.2.1 Documents
data Document annotation
    = SectionDocument    (Section annotation)
    | ExpressionDocument (Expression annotation)
    deriving (Show, Read, Eq, Data, Typeable, Generic)


-- 12.2.2 Section Documents
data Section annotation
    = Section
    { _section_attributes :: !(Maybe (RecordLiteral annotation))
    , _section_name       :: !(Maybe Identifier)
    , _section_members    :: !(Maybe [SectionMember annotation])
    , _section_annotation :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

data SectionMember annotation
    = SectionMember
    { _sectionMember_attributes :: !(Maybe (RecordLiteral annotation))
    , _sectionMember_shared     :: !Bool
    , _sectionMember_name       :: !Identifier
    , _sectionMember_expression :: !(Expression annotation)
    , _sectionMember_annotation :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)


-- 12.2.3 Expressions Documents

-- 12.2.3.1 Expressions
data Expression annotation
    = LogicalE       (LogicalOrExpression annotation)
    | EachE          (EachExpression annotation)
    | FunctionE      (FunctionExpression annotation)
    | LetE           (LetExpression annotation)
    | IfE            (IfExpression annotation)
    | ErrorRaisingE  (ErrorRaisingExpression annotation)
    | ErrorHandlingE (ErrorHandlingExpression annotation)
    deriving (Show, Read, Eq, Data, Typeable, Generic)


-- 12.2.3.2 Logical Expressions
data LogicalOrExpression annotation
    = And_OE (LogicalAndExpression annotation)
    | Or_OE  (LogicalAndExpression annotation)
             (LogicalOrExpression  annotation)
    deriving (Show, Read, Eq, Data, Typeable, Generic)

data LogicalAndExpression annotation
    = Is_LAE  (IsExpression annotation)
    | And_LAE (LogicalAndExpression annotation)
             (IsExpression  annotation)
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.3.3 Is expression
data IsExpression annotation
    = As_IE (AsExpression annotation)
    | Is_IE (IsExpression annotation) NullablePrimitiveType
    deriving (Show, Read, Eq, Data, Typeable, Generic)

data NullablePrimitiveType
    = NullablePrimitiveType
    { _nullablePrimitiveType_type :: !PrimitiveType
    , _nullablePrimitiveType_nullable :: !Bool
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.3.4 As expression
data AsExpression annotation
    = EqualityAE (EqualityExpression annotation)
    | As_AE      (AsExpression annotation) NullablePrimitiveType
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.3.5 Equality expression
data EqualityExpression annotation
    = RelationalEE (RelationalExpression annotation)
    | Eq_EE (RelationalExpression annotation)
            (EqualityExpression annotation)
    | Neq_EE (RelationalExpression annotation)
             (EqualityExpression annotation)
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.3.6 Relational jxpression
data RelationalExpression annotation
    = AdditiveRE (AdditiveExpression annotation)
    | Lt_RE       (AdditiveExpression annotation)
                  (RelationalExpression annotation)
    | Gt_RE       (AdditiveExpression annotation)
                  (RelationalExpression annotation)
    | Leq_RE      (AdditiveExpression annotation)
                  (RelationalExpression annotation)
    | Geq_RE      (AdditiveExpression annotation)
                  (RelationalExpression annotation)
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.3.7 Arithmetic kxpression
data AdditiveExpression annotation
    = MultiplicativeAE (MultiplicativeExpression annotation)
    | PlusAE  (MultiplicativeExpression annotation)
              (AdditiveExpression annotation)
    | MinusAE (MultiplicativeExpression annotation)
              (AdditiveExpression annotation)
    | And_AE   (MultiplicativeExpression annotation)
              (AdditiveExpression annotation)
    deriving (Show, Read, Eq, Data, Typeable, Generic)

data MultiplicativeExpression annotation
    = MetadataME (MetadataExpression annotation)
    | MultME (MetadataExpression annotation)
             (MultiplicativeExpression annotation)
    | DivME  (MetadataExpression annotation)
             (MultiplicativeExpression annotation)
    deriving (Show, Read, Eq, Data, Typeable, Generic)


-- 12.2.3.8 Metadata expression
data MetadataExpression annotation
    = MetadataExpression
    { _metadataExpression_first      :: !(UnaryExpression annotation)
    , _metadataExpression_second     :: !(Maybe (UnaryExpression annotation))
    , _metadataExpression_annotation :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.3.9 Unary expression
data UnaryExpression annotation
    = UnaryType  (TypeExpression annotation)
    | UnaryPlus  (UnaryExpression annotation)
    | UnaryMinus (UnaryExpression annotation)
    | UnaryNot   (UnaryExpression annotation)
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.3.10 Primary expression
data PrimaryExpression annotation
    = LiteralPE        (LiteralExpression annotation)
    | ListPE           (ListExpression annotation)
    | RecordPE         (RecordExpression annotation)
    | IdentifierPE     (IdentifierExpression annotation)
    | SectionAccessPE  (SectionAccessExpression annotation)
    | ParenthesizedPE  (ParenthesizedExpression annotation)
    | FieldAccessPE    (FieldAccessExpression annotation)
    | ItemAccessPE     (ItemAccessExpression annotation)
    | InvokePE         (InvokeExpression annotation)
    | NotImplementedPE (NotImplementedExpression annotation)
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.3.11 Literal expression
data LiteralExpression annotation
    = LiteralExpression
    { _literalExpression_literal    :: !Literal
    , _literalExpression_annotation :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.3.12 Identifier Expression
data IdentifierExpression annotation
    = IdentifierExpression
    { _identifierExpression_reference  :: !Identifier
    , _identifierExpression_inclusive  :: !Bool
    , _identifierExpression_annotation :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.3.13 Section-access expressoin
data SectionAccessExpression annotation
    = SectionAccessExpression
    { _sectionAccessExpression_sectionName :: !Identifier
    , _sectionAccessExpression_valueName   :: !Identifier
    , _sectionAccessExpression_annotation  :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.3.14 Parenthesized expression
data ParenthesizedExpression annotation
    = ParenthesizedExpression
    { _parenthesizedExpression_expression :: !(Expression annotation)
    , _parenthesizedExpression_annotation :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.3.15 NotImplement expression
data NotImplementedExpression annotation
    = NotImplementedExpression
    { _notImplementedExpression_annotation :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.3.16 Invokte expression
data InvokeExpression annotation
    = InvokeExpression
    { _invokeExpression_primary      :: !(PrimaryExpression annotation)
    , _invokeExpression_argumentList :: !(Maybe [Expression annotation])
    , _invokeExpression_annotation   :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.3.17 List expression
data ListExpression annotation
    = ListExpression
    { _listExpression_itemList   :: !(Maybe [Item annotation])
    , _listExpression_annotation :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

data Item annotation
    = Item
    { _item_first  :: !(Expression annotation)
    , _item_second :: !(Maybe (Expression annotation))
    , _item_annotation :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.3.18 Record expression
data RecordExpression annotation
    = RecordExpression
    { _recordExpression_fieldList  :: !(Maybe [Field annotation])
    , _recordExpression_annotation :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

data Field annotation
    = Field
    { _field_name       :: !Identifier
    , _field_expression :: !(Expression annotation)
    , _field_annotation :: (Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.3.19 Item access expression
data ItemAccessExpression annotation
    = ItemAccessExpression
    { _itemAccessExpression_selection    :: !(PrimaryExpression annotation)
    , _itemAccessExpression_itemSelector :: !(Expression annotation)
    , _itemAccessExpression_optional     :: !Bool
    , _itemAccessExpression_annotation   :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)


-- 12.2.3.20 Field access expressions
data FieldAccessExpression annotation
    = FieldSelection
      { _fieldSelection_primary       :: !(PrimaryExpression annotation)
      , _fieldSelection_fieldSelector :: !(FieldSelector annotation)
      , _fieldSelection_anotation     :: !(Maybe annotation)
      }
    | ImplicitTargetFieldSelection
      { _implicitTargetFieldSelection_fieldSelector :: !(FieldSelector annotation)
      , _implicitTargetFieldSelection_annotation    :: !(Maybe annotation)
      }
    | Projection
      { _projection_primary      :: !(PrimaryExpression annotation)
      , _projection_selectorList :: !([FieldSelector annotation])
      , _projection_optional     :: !Bool
      , _projection_annotation   :: !(Maybe annotation)
      }
    | ImplicitTargetProjection
      { _implicitTargetProjection_identifier   :: !Identifier
      , _implicitTargetProjection_annotation   :: !(Maybe annotation)
      }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

data FieldSelector annotation
    = FieldSelector
    { _fieldSelector_fieldName  :: !Identifier
    , _fieldSelector_optional   :: !Bool
    , _fieldSelector_annotation :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)


-- 12.2.3.21 Function expression
data FunctionExpression annotation
    = FunctionExpression
    { _functionExpression_body          :: !(Expression annotation)
    , _functionExpression_parameterList :: !(Maybe [Parameter annotation])
    , _functionExpression_return        :: !(Maybe (Type annotation))
    , _functionExpression_annotation    :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

data Parameter annotation
    = Parameter
    { _parameter_name       :: !Identifier
    , _parameter_type       :: !(Maybe (Type annotation))
    , _parameter_optional   :: !Bool
    , _parameter_annotation :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.3.22 Each expression
data EachExpression annotation
    = EachExpression
    { _eachExpression_body       :: !(Expression annotation)
    , _eachExpression_annotation :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.3.23 Let expression
data LetExpression annotation
    = LetExpression
    { _letExpression_variableList :: ![Variable annotation]
    , _letExpression_expression   :: !(Expression annotation)
    , _letExpression_annotation   :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

data Variable annotation
    = Variable
    { _variable_name       :: !Identifier
    , _variable_expression :: !(Expression annotation)
    , _variable_annotation :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.3.24 If Expression
data IfExpression annotation
    = IfExpression
    { _ifExpression_condition  :: !(Expression annotation)
    , _ifExpression_true       :: !(Expression annotation)
    , _ifExpression_false      :: !(Expression annotation)
    , _ifExpression_annotation :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.3.2.25 Type Expression
data TypeExpression annotation
    = Primary_TE (PrimaryExpression annotation)
    | PrimaryType_TE (PrimaryType annotation)
    deriving (Show, Read, Eq, Data, Typeable, Generic)


data Type annotation
    = ParenthesizedT (ParenthesizedExpression annotation)
    | TypeT
      { _type_primary    :: !(PrimaryType annotation)
      , _type_annotation :: !(Maybe annotation)
      }
    deriving (Show, Read, Eq, Data, Typeable, Generic)


data PrimaryType annotation
    = PrimitiveType' PrimitiveType
    | RecordType
      { _recordType_fieldSpecifications :: !([FieldSpecification annotation])
      , _recordType_annotation          :: !(Maybe annotation)
      }
    | ListType
      { _listType_itemType   :: !(Type annotation)
      , _listType_annotation :: !(Maybe annotation)
      }
    | FunctionType
      { _functionType_parametersSpecifications :: !(Maybe [ParameterSpecification annotation])
      , _functionType_returnType               :: !(Type annotation)
      , _functionType_annotation               :: !(Maybe annotation)
      }
    | TableType
      { _tableType_rowTypes   :: !([FieldSpecification annotation])
      , _tableType_annotation :: !(Maybe annotation)
      }
    | NullableType
      { _nullableType_type       :: !(Type annotation)
      , _nullableType_annotation :: !(Maybe annotation)
      }
    deriving (Show, Read, Eq, Data, Typeable, Generic)


data FieldSpecification annotation
    = FieldSpecification
    { _fieldSpecification_identifier :: !Identifier
    , _fieldSpecification_fieldType  :: !(Maybe (Type annotation))
    , _fieldSpecification_optional   :: !Bool
    , _fieldSpecification_annotation :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)


data ParameterSpecification annotation
    = ParameterSpecification
    { _parameterSpecification_name       :: !Identifier
    , _parameterSpecification_type       :: !(Maybe (Type annotation)) -- TOOD: ParameterType
    , _parameterSpecification_optional   :: !Bool
    , _parameterSpecification_annotation :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)


data PrimitiveType
    = TypeAny
    | TypeAnyNonNull
    | TypeBinary
    | TypeDate
    | TypeDateTime
    | TypeDateTimezone
    | TypeDuration
    | TypeFunction
    | TypeList
    | TypeLogical
    | TypeNone
    | TypeNull
    | TypeNumber
    | TypeRecord
    | TypeTable
    | TypeText
    | TypeType
    deriving (Show, Read, Eq, Ord, Bounded, Data, Typeable, Generic)


-- 12.2.3.26 Error raising expression
data ErrorRaisingExpression annotation
    = ErrorRaisingExpression
    { _errorRaisingExpression_error      :: !(Expression annotation)
    , _errorRaisingExpression_annotation :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.3.27 Error handling expression
data ErrorHandlingExpression annotation
    = ErrorHandlingExpression
    { _errorHandlingExpression_protected  :: !(Expression annotation)
    , _errorHandlingExpression_otherwise  :: !(Maybe (Expression annotation))
    , _errorHandlingExpression_annotation :: !(Maybe annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

-- 12.2.4 Literal Attributes
data RecordLiteral annotation
    = RecordLiteral' (Maybe [LiteralField annotation])
    deriving (Show, Read, Eq, Data, Typeable, Generic)

data LiteralField annotation
    = LiteralField
    { _literalField_name    :: !Identifier
    , _literalField_literal :: !(AnyLiteral annotation)
    }
    deriving (Show, Read, Eq, Data, Typeable, Generic)

data ListLiteral annotation
    = ListLiteral' (Maybe [AnyLiteral annotation])
    deriving (Show, Read, Eq, Data, Typeable, Generic)

data AnyLiteral annotation
    = Record_AL  (RecordLiteral annotation)
    | List_AL    (ListLiteral annotation)
    | Literal_AL (Literal)
    deriving (Show, Read, Eq, Data, Typeable, Generic)

