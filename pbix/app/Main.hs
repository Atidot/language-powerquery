{-# LANGUAGE PackageImports #-}
module Main where

import           "base"                    Data.Semigroup ((<>))
import           "base"                    System.IO (stdin)
import           "lens"                    Control.Lens
import qualified "bytestring"              Data.ByteString.Lazy.Char8 as B (ByteString, readFile, hGetContents, putStrLn)
import           "aeson"                   Data.Aeson (encode)
import           "optparse-applicative"    Options.Applicative
import           "pbix"                    Codec.Pbix.Types
import           "language-powerquery-ast" Language.PowerQuery.AST


data ListFormulas
    = ListFormulas
    { _listFormula_path :: !(Maybe FilePath)
    }

data PrintFormula
    = PrintFormula
    { _printFormula_path    :: !(Maybe FilePath)
    , _printFormula_formula :: !String
    , _printFormula_parse   :: !Bool
    , _printFormaul_json    :: !Bool
    }

data Command
    = C1 ListFormulas
    | C2 PrintFormula

listFormulasParser :: Parser Command
listFormulasParser = C1 <$> (ListFormulas
    <$> optional (strOption
      ( long "path"
     <> short 'p'
     <> metavar "PATH"
     <> help ".pbix file path"
      )
    ))

printFormulaParser :: Parser Command
printFormulaParser = C2 <$> (PrintFormula
    <$> optional (strOption
      ( long "path"
     <> short 'p'
     <> metavar "PATH"
     <> help ".pbix file path"
      ))
    <*> strOption
      ( long "formula"
     <> short 'f'
     <> metavar "FORMULA"
     <> help "formula (M script) name"
      )
    <*> switch
      ( long "parse"
     <> short 'x'
     <> help "should parse to an AST"
      )
    <*> switch
      ( long "json"
     <> short 'j'
     <> help "print AST as JSON"
      )
    )


combined :: Parser Command
combined = subparser
         ( command "list" (info listFormulasParser (progDesc "List formulas in .pbix"))
        <> command "print" (info printFormulaParser (progDesc "Print a formula in a .pbix"))
         )

readPathOrStdin :: Maybe FilePath -> IO B.ByteString
readPathOrStdin Nothing     = B.hGetContents stdin
readPathOrStdin (Just path) = B.readFile path

listFormulas :: ListFormulas -> IO ()
listFormulas (ListFormulas mPath) = do
    bs <- readPathOrStdin mPath
    print $ bs ^. pbix . dataMashup . formulas

printFormula :: PrintFormula -> IO ()
printFormula (PrintFormula mPath name shouldParse shouldJSON) = do
    bs <- readPathOrStdin mPath
    let script = bs ^. pbix . dataMashup . formula name
    if shouldParse
    then if shouldJSON
         then B.putStrLn . encode $ script ^. document
         else print          $ script ^. document
    else do
        let (Formula text) = script
        B.putStrLn text

main :: IO ()
main = do
    options <- execParser (info (combined <**> helper) (fullDesc <> progDesc "PowerBI .pbix file analyzer"))
    case options of
        C1 config -> listFormulas config
        C2 config -> printFormula config
