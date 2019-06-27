{-# LANGUAGE PackageImports #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE InstanceSigs #-}
module Codec.Pbix.Types.Lens where

import           "base"                    Data.Maybe (fromJust)
import           "lens"                    Control.Lens
import qualified "text"                    Data.Text as T
import qualified "bytestring"              Data.ByteString.Lazy.Char8 as BL8
import           "zip-archive"             Codec.Archive.Zip
import           "language-powerquery-ast" Language.PowerQuery.AST
import           "language-powerquery"     Language.PowerQuery
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
            unknownHeader :: BL8.ByteString
            unknownHeader = "\x00\x00\x00\x00\x00\x00\x00\x00"

            getter (Pbix archive)
                = DataMashup
                . toArchive
                . BL8.drop (BL8.length unknownHeader)
                . fromJust
                . (fromEntry <$>)
                . findEntryByPath "DataMashup"
                $ archive

            setter (Pbix archive) (DataMashup innerArchive)
                = Pbix
                . flip addEntryToArchive archive
                . toEntry "DataMashup" 0
                . (unknownHeader <>)
                . fromArchive
                $ innerArchive


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

            setter (DataMashup archive) (Formula bs)
                = DataMashup
                . flip addEntryToArchive archive
                . toEntry ("Formulas/" <> name) 0
                $ bs


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

            setter (Formula bs) doc
                = Formula
                . BL8.pack
                . T.unpack
                . pprint
                $ doc
