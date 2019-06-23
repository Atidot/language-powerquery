{-# LANGUAGE PackageImports #-}
module Main where

import "hspec"               Test.Hspec hiding (example)
--import "language-powerquery" Language.PowerQuery.AST.Annotation
--import "language-powerquery" Language.PowerQuery.AST.Token
--import "language-powerquery" Language.PowerQuery.AST.AST
import "language-powerquery" Language.PowerQuery.Lexer (lexer)
import "language-powerquery" Language.PowerQuery.Parser


main :: IO ()
main = hspec $ do
    describe "Parser sanity tests" $ do
        it "Parses 'Document'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'SectionDocument'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'Section'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'SectionName" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'SectionMembers" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'SectionMember'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'SectionMemberName'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'ExpressionDocument" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'Expression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'LogicalOrExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'LogicalAndExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'IsExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'NullalblePrimitiveType'" $do
            1 `shouldBe` (1 :: Int)

        it "Parses 'AsExpression'" $do
            1 `shouldBe` (1 :: Int)

        it "Parses 'EqualityExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'RelationalExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'AdditiveExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'MultiplicativeExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'MetadataExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'UnaryExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'PrimaryExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'LiteralExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'IdentifierExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'IdentifierReference'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'ExclusiveIdentifierReference'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'InclusiveIdentifierReference'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'SectionAccessExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'ParenthesizedExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'NotImplementedExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'InvokeExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'ArgumentList'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'ListExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'ItemList'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'Item'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'RecordExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'FieldList'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'Field'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'FieldName'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'ItemAccessExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'ItemSelection'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'OptionalItemSelection'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'ItemSelector'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'FieldAccessExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'FieldSelection'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'FieldSelector'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'RequiredFieldSelector'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'OptionalFieldSelector'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'ImplicitTargetFieldSelection'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'Projection'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'RequiredProjection'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'OptionalProjection'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'RequiredSelectorList'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'ImplicitTargetProjection'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'FunctionExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'FunctionBody'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'ParameterList'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'FixedParameterList'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'Parameter'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'ParameterName'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'ParameterType'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'ReturnType'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'Assertion'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'OptionalParameterList'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'OptionalParameter'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'EachExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'EachExpressionBody'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'LetExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'VariableList'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'Variable'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'VariableName'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'IfExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'IfCondition'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'TrueExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'FalseExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'TypeExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'Type'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'PrimaryType'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'PrimitiveType'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'RecordType'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'ListType'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'FunctionType'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'TableType'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'NullableType'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'ErrorRaisingExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'ErrorHandlingExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'ProtectedExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'OtherwiseClause'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'DefaultExpression'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'LiteralAttributes'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'RecordLiteral'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'LiteralFieldList'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'LiteralField'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'ListLiteral'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'LiteramItemList'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'AnyLiteral'" $ do
            1 `shouldBe` (1 :: Int)

        it "Parses 'LogicalLiteral'" $ do
            1 `shouldBe` (1 :: Int)
