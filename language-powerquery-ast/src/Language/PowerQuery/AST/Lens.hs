{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.PowerQuery.AST.Lens where

import "base" Data.Data (Data)
import "lens" Control.Lens
import        Language.PowerQuery.AST.Token
import        Language.PowerQuery.AST.AST

makeClassy       ''Token
makeClassyPrisms ''Token
makeClassy       ''Literal
makeClassyPrisms ''Literal
makeClassy       ''NumberLiteral
makeClassyPrisms ''NumberLiteral
makeClassy       ''Identifier
makeClassyPrisms ''Identifier
makeClassy       ''Keyword
makeClassyPrisms ''Keyword
makeClassy       ''Operator
makeClassyPrisms ''Operator

makeClassy       ''Document
makeClassyPrisms ''Document
makeClassy       ''Section
makeClassy       ''SectionMember
makeClassy       ''Expression
--makeClassyPrisms ''Expression
makeClassy       ''LogicalOrExpression
makeClassyPrisms ''LogicalOrExpression
makeClassy       ''LogicalAndExpression
makeClassyPrisms ''LogicalAndExpression
makeClassy       ''IsExpression
makeClassyPrisms ''IsExpression
makeClassy       ''AsExpression
makeClassyPrisms ''AsExpression
makeClassy       ''EqualityExpression
makeClassyPrisms ''EqualityExpression
makeClassy       ''RelationalExpression
makeClassyPrisms ''RelationalExpression
makeClassy       ''AdditiveExpression
makeClassyPrisms ''AdditiveExpression
makeClassy       ''MultiplicativeExpression
makeClassyPrisms ''MultiplicativeExpression
makeClassy       ''MetadataExpression
makeClassy       ''UnaryExpression
makeClassyPrisms ''UnaryExpression
makeClassy       ''PrimaryExpression
makeClassyPrisms ''PrimaryExpression
makeClassy       ''LiteralExpression
makeClassy       ''IdentifierExpression
makeClassy       ''SectionAccessExpression
makeClassy       ''ParenthesizedExpression
makeClassy       ''NotImplementedExpression
makeClassy       ''InvokeExpression
makeClassy       ''ListExpression
makeClassy       ''Item
makeClassy       ''RecordExpression
makeClassy       ''Field
makeClassy       ''ItemAccessExpression
makeClassy       ''FieldAccessExpression
makeClassyPrisms ''FieldAccessExpression
makeClassy       ''FieldSelector
makeClassy       ''FunctionExpression
makeClassy       ''Parameter
makeClassy       ''EachExpression
makeClassy       ''LetExpression
makeClassy       ''Variable
makeClassy       ''IfExpression
makeClassy       ''TypeExpression
makeClassyPrisms ''TypeExpression
--makeClassy ''Type
makeClassy       ''PrimaryType
makeClassyPrisms ''PrimaryType
makeClassy       ''FieldSpecification
makeClassy       ''ParameterSpecification
makeClassy       ''PrimitiveType
makeClassyPrisms ''PrimitiveType
makeClassy       ''ErrorRaisingExpression
makeClassy       ''ErrorHandlingExpression
makeClassy       ''RecordLiteral
makeClassyPrisms ''RecordLiteral
makeClassy       ''LiteralField
makeClassy       ''ListLiteral
makeClassyPrisms ''ListLiteral
makeClassy       ''AnyLiteral
makeClassyPrisms ''AnyLiteral


instance Plated Token
instance Plated Literal
instance Plated Identifier
instance Plated Keyword
instance Plated Operator

instance (Data a) => Plated (Document a)
instance (Data a) => Plated (Section a)
instance (Data a) => Plated (SectionMember a)
instance (Data a) => Plated (Expression a)
instance (Data a) => Plated (LogicalOrExpression a)
instance (Data a) => Plated (LogicalAndExpression a)
instance (Data a) => Plated (IsExpression a)
instance (Data a) => Plated (AsExpression a)
instance (Data a) => Plated (EqualityExpression a)
instance (Data a) => Plated (RelationalExpression a)
instance (Data a) => Plated (AdditiveExpression a)
instance (Data a) => Plated (MultiplicativeExpression a)
instance (Data a) => Plated (MetadataExpression a)
instance (Data a) => Plated (UnaryExpression a)
instance (Data a) => Plated (PrimaryExpression a)
instance (Data a) => Plated (LiteralExpression a)
instance (Data a) => Plated (IdentifierExpression a)
instance (Data a) => Plated (SectionAccessExpression a)
instance (Data a) => Plated (ParenthesizedExpression a)
instance (Data a) => Plated (NotImplementedExpression a)
instance (Data a) => Plated (InvokeExpression a)
instance (Data a) => Plated (ListExpression a)
instance (Data a) => Plated (Item a)
instance (Data a) => Plated (RecordExpression a)
instance (Data a) => Plated (Field a)
instance (Data a) => Plated (ItemAccessExpression a)
instance (Data a) => Plated (FieldAccessExpression a)
instance (Data a) => Plated (FieldSelector a)
instance (Data a) => Plated (FunctionExpression a)
instance (Data a) => Plated (Parameter a)
instance (Data a) => Plated (EachExpression a)
instance (Data a) => Plated (LetExpression a)
instance (Data a) => Plated (Variable a)
instance (Data a) => Plated (IfExpression a)
instance (Data a) => Plated (TypeExpression a)
--instance Plated Type a)
instance (Data a) => Plated (PrimaryType a)
instance (Data a) => Plated (FieldSpecification a)
instance (Data a) => Plated (ParameterSpecification a)
instance             Plated PrimitiveType
instance (Data a) => Plated (ErrorRaisingExpression a)
instance (Data a) => Plated (ErrorHandlingExpression a)
instance (Data a) => Plated (RecordLiteral a)
instance (Data a) => Plated (LiteralField a)
instance (Data a) => Plated (ListLiteral a)
instance (Data a) => Plated (AnyLiteral a)
