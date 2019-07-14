{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
module Language.PowerQuery.Editor.Editor where

import                                     Prelude hiding (head)
import           "base"                    Control.Monad.IO.Class (liftIO)
import           "lens"                    Control.Lens
import           "data-default"            Data.Default (def)
import           "uniplate"                Data.Generics.Uniplate.Data (universeBi)
import           "aeson"                   Data.Aeson (Value(Null), toJSON)
import           "text"                    Data.Text (Text, unpack, pack)
import           "time"                    Data.Time.Clock (getCurrentTime)
import           "reflex"                  Reflex
import           "reflex-dom"              Reflex.Dom hiding (Value)

import           "reflex-utils"            Reflex.Utils
import           "reflex-mdl"              Reflex.MDL
import           "reflex-chartjs"          Reflex.ChartJS.ChartJS
import           "reflex-codemirror"       Reflex.CodeMirror
import           "reflex-jsoneditor"       Reflex.JsonEditor
import           "reflex-select2"          Reflex.Select2.Select2
import           "language-powerquery-ast" Language.PowerQuery.AST
import           "language-powerquery"     Language.PowerQuery

--
main :: IO ()
main = mainWidget main_
    where
        main_ :: forall t m. MonadWidget t m => m ()
        main_ = do
            headD <- head
            whenLoaded [headD] blank body
            return ()


--
head :: forall t m. MonadWidget t m => m (Dynamic t Bool)
head = do
    s1Ds <- sequence [ script "https://cdnjs.cloudflare.com/ajax/libs/jquery/3.3.1/jquery.min.js"
                     , script "https://cdnjs.cloudflare.com/ajax/libs/Chart.js/2.5.0/Chart.bundle.min.js"

                     , script "https://cdnjs.cloudflare.com/ajax/libs/jsoneditor/6.1.0/jsoneditor.min.js"
                     , css    "https://cdnjs.cloudflare.com/ajax/libs/jsoneditor/6.1.0/jsoneditor.min.css"

                     , script "https://cdnjs.cloudflare.com/ajax/libs/material-design-lite/1.3.0/material.min.js"
                     , css    "https://cdnjs.cloudflare.com/ajax/libs/material-design-lite/1.3.0/material.min.css"
                     , script "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.48.0/codemirror.min.js"
                     , css    "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.48.0/codemirror.min.css"
                     , css    "https://cdnjs.cloudflare.com/ajax/libs/codemirror/5.48.0/theme/zenburn.css"
                     ]
    whenLoaded s1Ds blank $ do
        sequence [ script "https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.7/js/select2.full.min.js"
                 , css    "https://cdnjs.cloudflare.com/ajax/libs/select2/4.0.7/css/select2.css.min"
                 ]
        return ()


--
body :: forall t m. (MonadWidget t m)
     => m ()
body = do
    mdlGrid $ do
        -- code editor
        scriptE <- mdlCell 6 $ do
            (scriptE' :: Event t Text) <- codemirror codeMirrorConfig
            debounce 0.05 scriptE' -- rate limit change events from editor

        -- Tokens
        let lexE = (lexer . unpack) <$> scriptE
        let (tokensE   :: Event t [Token]) = filterRight lexE
        let (lexErrorE :: Event t String)  = filterLeft  lexE

        -- AST
        let (astE :: Event t (Document Annotation)) = parseDocument <$> tokensE

        let (variablesE :: Event t [Text]) = (map (pack . show) . map _variable_name . variables) <$> astE
        variablesD <- holdDyn [] variablesE

        -- JSON viewer
        _ <- mdlCell 6 $ do
            -- variables
            display variablesD
            --_ <- select2 def variablesD

            -- JSON editor
            let (jsonE :: Event t Value) = toJSON <$> astE
            jsonD <- holdDyn Null jsonE
            _ <- jsoneditor jsonD
            blank

        return ()

    where
        variables :: (Document Annotation -> [Variable Annotation])
        variables = universeBi

        codeMirrorConfig :: Configuration
        codeMirrorConfig
            = def
            & configuration_theme ?~ pack "zenburn"
