{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Language.PowerQuery.AST.Arbitrary where

import "text"              Data.Text (Text)
import "generic-arbitrary" Test.QuickCheck.Arbitrary.Generic
import "quickcheck-text"   Test.QuickCheck.Utf8
import                     Language.PowerQuery.AST.Token
import                     Language.PowerQuery.AST.Annotation
import                     Language.PowerQuery.AST.AST

instance Arbitrary Text where
    arbitrary = genValidUtf8
    shrink    = shrinkValidUtf8

instance Arbitrary Token where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary Literal where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary NumberLiteral where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary Identifier where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary Keyword where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary Operator where
    arbitrary = genericArbitrary
    shrink    = genericShrink

--
instance Arbitrary Annotation where
    arbitrary = genericArbitrary
    shrink    = genericShrink

--
instance (Arbitrary a) => Arbitrary (Document a) where
    arbitrary = genericArbitrary
    shrink   = genericShrink

instance (Arbitrary a) => Arbitrary (Section a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (SectionMember a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (Expression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (LogicalOrExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (LogicalAndExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (IsExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary (NullablePrimitiveType) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (AsExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (EqualityExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (RelationalExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (AdditiveExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (MultiplicativeExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (MetadataExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (UnaryExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (PrimaryExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (LiteralExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (IdentifierExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (SectionAccessExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (ParenthesizedExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (NotImplementedExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (InvokeExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (ListExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (Item a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (RecordExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (Field a) where
    arbitrary = genericArbitrary
    shrink   = genericShrink

instance (Arbitrary a) => Arbitrary (ItemAccessExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (FieldAccessExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (FieldSelector a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (FunctionExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (Parameter a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (EachExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (LetExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (Variable a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (IfExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (TypeExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (Type a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (PrimaryType a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (FieldSpecification a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (ParameterSpecification a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance Arbitrary PrimitiveType where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (ErrorRaisingExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (ErrorHandlingExpression a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (RecordLiteral a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (LiteralField a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (ListLiteral a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

instance (Arbitrary a) => Arbitrary (AnyLiteral a) where
    arbitrary = genericArbitrary
    shrink    = genericShrink

