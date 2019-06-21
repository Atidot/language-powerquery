{-# LANGUAGE PackageImports #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes #-}

module Language.PowerQuery.AST.Lens where

import "lens" Control.Lens
import        Language.PowerQuery.AST.Token
import        Language.PowerQuery.AST.AST

makeClassy ''Token
makeClassy ''Literal
makeClassy ''Identifier
makeClassy ''Keyword
makeClassy ''Operator

makeClassy ''Document
makeClassy ''Section
makeClassy ''SectionMember
makeClassy ''Expression
makeClassy ''LogicalOrExpression
makeClassy ''LogicalAndExpression
makeClassy ''IsExpression
makeClassy ''AsExpression
makeClassy ''EqualityExpression
makeClassy ''RelationalExpression
makeClassy ''AdditiveExpression
makeClassy ''MultiplicativeExpression
makeClassy ''MetadataExpression
makeClassy ''UnaryExpression
makeClassy ''PrimaryExpression
makeClassy ''LiteralExpression
makeClassy ''IdentifierExpression
makeClassy ''SectionAccessExpression
makeClassy ''ParenthesizedExpression
makeClassy ''NotImplementedExpression
makeClassy ''InvokeExpression
makeClassy ''ListExpression
makeClassy ''Item
makeClassy ''RecordExpression
makeClassy ''Field
makeClassy ''ItemAccessExpression
makeClassy ''FieldAccessExpression
makeClassy ''FieldSelector
makeClassy ''FunctionExpression
makeClassy ''Parameter
makeClassy ''EachExpression
makeClassy ''LetExpression
makeClassy ''Variable
makeClassy ''IfExpression
makeClassy ''TypeExpression
--makeClassy ''Type
makeClassy ''PrimaryType
makeClassy ''FieldSpecification
makeClassy ''ParameterSpecification
makeClassy ''PrimitiveType
makeClassy ''ErrorRaisingExpression
makeClassy ''ErrorHandlingExpression
makeClassy ''RecordLiteral
makeClassy ''LiteralField
makeClassy ''ListLiteral
makeClassy ''AnyLiteral
