{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
module Codec.Pbix.Types.Lens where

import           "base"                    Data.Maybe (fromJust)
import           "lens"                    Control.Lens
import qualified "bytestring"              Data.ByteString.Lazy.Char8 as BL8
import           "zip-archive"             Codec.Archive.Zip
import           "language-powerquery-ast" Language.PowerQuery.AST
import                                     Language.PowerQuery.Lexer  (lexer)
import                                     Language.PowerQuery.Parser (parseDocument)
import                                     Codec.Pbix.Types.Types


class HasPbix a where
    pbix :: Lens' a Pbix

class HasDataMashup a where
    dataMashup :: Lens' a DataMashup

class HasFormulas a where
    formula :: String -> Lens' a Formula


instance HasPbix BL8.ByteString where
    pbix :: Lens' BL8.ByteString Pbix
    pbix = lens getter setter
        where
            getter bs
                = Pbix
                . toArchive
                $ bs

            setter bs (Pbix archive)
                = fromArchive
                $ archive


instance HasDataMashup Pbix where
    dataMashup :: Lens' Pbix DataMashup
    dataMashup = lens getter setter
        where
            getter (Pbix archive)
                = DataMashup
                . toArchive
                . BL8.drop 8 -- unknown header...
                . fromJust
                . (fromEntry <$>)
                . findEntryByPath "DataMashup"
                $ archive

            setter pbix@(Pbix _) _ = pbix -- id


instance HasFormulas DataMashup where
    formula :: String -> Lens' DataMashup Formula
    formula name = lens getter setter
        where
            getter (DataMashup archive)
                = Formula
                . fromJust
                . (fromEntry <$>)
                . findEntryByPath ("Formulas/" <> name)
                $ archive

            setter dm@(DataMashup _) _ = dm -- id


instance HasDocument Formula Annotation where
    document :: Lens' Formula (Document Annotation)
    document = lens getter setter
        where
            fromRight (Right x)  = x
            fromRight (Left err) = error err

            getter (Formula bs)
                = parseDocument
                . fromRight
                . lexer
                . BL8.unpack
                $ bs

            setter formula@(Formula _) _ = formula -- id
