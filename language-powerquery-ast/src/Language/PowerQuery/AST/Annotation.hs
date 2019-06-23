{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleInstances #-}
module Language.PowerQuery.AST.Annotation where

import "base" GHC.Generics  (Generic)
import "base" Data.Typeable (Typeable)
import "base" Data.Data (Data)
import "lens" Control.Lens
import        Language.PowerQuery.AST.Lens
import        Language.PowerQuery.AST.AST

data Annotation = Annotation
    deriving (Show, Read, Data, Typeable, Generic)

class HasAnnotation a where
    annotation :: Lens' a (Maybe Annotation)

instance HasAnnotation (Section Annotation) where annotation = section_annotation
instance HasAnnotation (SectionMember Annotation) where annotation = sectionMember_annotation
--instance HasAnnotation (LogicalExpression Annotation) where annotation = 
--instance HasAnnotation (IsExpression Annotation) where annotation =
--instance HasAnnotation (AsExpression Annotation) where annotation =
--instance HasAnnotation (EqualityExpression Annotation) where annotation =
--instance HasAnnotation (RelationalExpression Annotation) where annotation =
--instance HasAnnotation (ArithmeticExpression Annotation) where annotation =
--instance HasAnnotation (MetadataExpression Annotation) where annotation =
--instance HasAnnotation (UnaryExpression Annotation) where annotation =
instance HasAnnotation (LiteralExpression Annotation) where annotation = literalExpression_annotation
instance HasAnnotation (IdentifierExpression Annotation) where annotation = identifierExpression_annotation
instance HasAnnotation (SectionAccessExpression Annotation) where annotation = sectionAccessExpression_annotation
instance HasAnnotation (ParenthesizedExpression Annotation) where annotation = parenthesizedExpression_annotation
instance HasAnnotation (NotImplementedExpression Annotation) where annotation = notImplementedExpression_annotation
instance HasAnnotation (InvokeExpression Annotation) where annotation = invokeExpression_annotation
instance HasAnnotation (ListExpression Annotation) where annotation = listExpression_annotation
instance HasAnnotation (Item Annotation) where annotation = item_annotation
instance HasAnnotation (RecordExpression Annotation) where annotation = recordExpression_annotation
instance HasAnnotation (Field Annotation) where annotation = field_annotation
instance HasAnnotation (ItemAccessExpression Annotation) where annotation = itemAccessExpression_annotation
--instance HasAnnotation (FieldAccessExpression Annotation) where annotation = field
instance HasAnnotation (FieldSelector Annotation) where annotation = fieldSelector_annotation
instance HasAnnotation (FunctionExpression Annotation) where annotation = functionExpression_annotation
instance HasAnnotation (Parameter Annotation) where annotation = parameter_annotation
instance HasAnnotation (EachExpression Annotation) where annotation = eachExpression_annotation
instance HasAnnotation (LetExpression Annotation) where annotation = letExpression_annotation
instance HasAnnotation (Variable Annotation) where annotation = variable_annotation
instance HasAnnotation (IfExpression Annotation) where annotation = ifExpression_annotation
--instance HasAnnotation (TypeExpression Annotation) where annotation =
--instance HasAnnotation (Type Annotation) where annotation = type_annotation
--instance HasAnnotation (PrimaryType Annotation) where annotation =
instance HasAnnotation (FieldSpecification Annotation) where annotation = fieldSpecification_annotation
instance HasAnnotation (ParameterSpecification Annotation) where annotation = parameterSpecification_annotation
instance HasAnnotation (ErrorRaisingExpression Annotation) where annotation = errorRaisingExpression_annotation
instance HasAnnotation (ErrorHandlingExpression Annotation) where annotation = errorHandlingExpression_annotation
