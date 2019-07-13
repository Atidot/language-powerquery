{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Language.PowerQuery.Editor.Editor where

import           "base"                    Control.Monad.IO.Class (liftIO)
import           "uniplate"                Data.Generics.Uniplate.Data (universeBi)
import           "aeson"                   Data.Aeson (Value(Null), toJSON)
import           "text"                    Data.Text (Text, unpack)
import           "time"                    Data.Time.Clock (getCurrentTime)
import           "reflex"                  Reflex
import           "reflex-dom"              Reflex.Dom hiding (Value)
import           "language-powerquery-ast" Language.PowerQuery.AST
import           "language-powerquery"     Language.PowerQuery

codemirror :: forall t m. (MonadWidget t m)
           => ()
           -> m (Event t Text)
codemirror _ = return undefined

jsoneditor :: forall t m. (MonadWidget t m)
           => Dynamic t Value
           -> m ()
jsoneditor _ = return ()


body :: forall t m. (MonadWidget t m)
     => m ()
body = do
    -- code editor
    (scriptE' :: Event t Text) <- codemirror ()
    scriptE <- debounce 1 scriptE' -- rate limit change events from editor

    -- Tokens
    let lexE = (lexer . unpack) <$> scriptE
    let (tokensE   :: Event t [Token]) = filterRight lexE
    let (lexErrorE :: Event t String)  = filterLeft  lexE

    -- AST
    let (astE :: Event t (Document Annotation))       = parseDocument <$> tokensE
    let (variablesE :: Event t [Variable Annotation]) = variables <$> astE

    -- JSON viewer
    let (jsonE :: Event t Value) = toJSON <$> astE
    jsonD <- holdDyn Null jsonE
    jsoneditor jsonD

    where
        variables :: (Document Annotation -> [Variable Annotation])
        variables = universeBi



main = mainWidget $ do
    body
