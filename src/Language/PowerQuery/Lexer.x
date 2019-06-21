{
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.PowerQuery.Lexer where

import "base"        Data.Typeable (Typeable)
import "base"        Data.Data (Data)
import "base"        Control.Monad.IO.Class (liftIO)
import "text"        Data.Text (Text, pack)
import "monad-loops" Control.Monad.Loops (whileJust)
import               Language.PowerQuery.Token
}

%wrapper "monad"


$whitechar = [ \t\n\r\f\v]
$special   = [\(\)\,\;\[\]\`\{\}]

$ascdigit  = 0-9
$unidigit  = [] -- TODO
$digit     = [$ascdigit $unidigit]

$ascsymbol = [\!\#\$\%\&\*\+\.\/\<\=\>\?\@\\\^\|\-\~]
$unisymbol = [] -- TODO
$symbol    = [$ascsymbol $unisymbol] # [$special \_\:\"\']
$large     = [A-Z \xc0-\xd6 \xd8-\xde]
$small     = [a-z \xdf-\xf6 \xf8-\xff \_]
$alpha     = [$small $large]
$graphic   = [$small $large $symbol $digit $special \:\"\']
$octit	   = 0-7
$hexit     = [0-9 A-F a-f]
$idchar    = [$alpha $digit \']
$symchar   = [$symbol \:]
$nl        = [\n\r]

@varid  = $small $idchar*
@conid  = $large $idchar*
@varsym = $symbol $symchar*
@consym = \: $symchar*
@decimal     = $digit+
@octal       = $octit+
@hexadecimal = $hexit+
@exponent    = [eE] [\-\+] @decimal
$cntrl   = [$large \@\[\\\]\^\_]
@ascii   = \^ $cntrl | NUL | SOH | STX | ETX | EOT | ENQ | ACK
	 | BEL | BS | HT | LF | VT | FF | CR | SO | SI | DLE
	 | DC1 | DC2 | DC3 | DC4 | NAK | SYN | ETB | CAN | EM
	 | SUB | ESC | FS | GS | RS | US | SP | DEL
$charesc = [abfnrtv\\\"\'\&]
@escape  = \\ ($charesc | @ascii | @decimal | o @octal | x @hexadecimal)
@gap     = \\ $whitechar+ \\
@string  = $graphic # [\"\\] | " " | @escape | @gap


powerquery :-
  <0> $white+  { skip }

  -- 12.1.2 - Comment
  <0> "//".*   { mkL TComment }

  --==========
  -- 12.1.5 - number-literal
  <0> @decimal
    | 0[xX] @hexadecimal     { mkInteger }

  <0> @decimal \. @decimal @exponent?
    | \. @decimal @exponent?
    | @decimal @exponent     { mkFloat }

  --==========
  -- 12.1.5 - text-literal
  <0> \" @string* \"         { mkString }

  --==========
  -- 12.1.5 - null-literal
  <0> null                   { mkL $ TLiteral Null }

  --==========
  -- 12.1.6 - quoted-identifer
  <0> \# \" @string* \"       { mkQuotedIdentifier }


  --==========
  -- 12.1.7 - Keywords and predefined identifiers
  "and"            { mkL $ TKeyword And }
  "as"             { mkL $ TKeyword As }
  "each"           { mkL $ TKeyword Each }
  "else"           { mkL $ TKeyword Else }
  "error"          { mkL $ TKeyword Error }
  "false"          { mkL $ TKeyword False' }
  "if"             { mkL $ TKeyword If }
  "in"             { mkL $ TKeyword In }
  "is"             { mkL $ TKeyword Is }
  "let"            { mkL $ TKeyword Let }
  "meta"           { mkL $ TKeyword Meta }
  "not"            { mkL $ TKeyword Not }
  "otherwise"      { mkL $ TKeyword Otherwise }
  "or"             { mkL $ TKeyword Or }
  "section"        { mkL $ TKeyword Section }
  "shared"         { mkL $ TKeyword Shared }
  "then"           { mkL $ TKeyword Then }
  "true"           { mkL $ TKeyword True' }
  "try"            { mkL $ TKeyword Try }
  "type"           { mkL $ TKeyword Type' }
  "#binary"        { mkL $ TKeyword H_Binary }
  "#date"          { mkL $ TKeyword H_Date }
  "#datetime"      { mkL $ TKeyword H_DateTime }
  "#datetimezone"  { mkL $ TKeyword H_DateTimezone }
  "#duration"      { mkL $ TKeyword H_Duration }
  "#infinity"      { mkL $ TKeyword H_Infinity }
  "#nan"           { mkL $ TKeyword H_Nan }
  "#sections"      { mkL $ TKeyword H_Sections }
  "#shared"        { mkL $ TKeyword H_Shared }
  "#table"         { mkL $ TKeyword H_Table }
  "#time"          { mkL $ TKeyword H_Time }


  --==========
  -- 12.1.8 - Operators and punctuators
  ","   { mkL $ TOperator Comma }
  ";"   { mkL $ TOperator SemiColon }
  "="   { mkL $ TOperator Equal }
  "<"   { mkL $ TOperator LT' }
  "<="  { mkL $ TOperator LEQ }
  ">"   { mkL $ TOperator GT' }
  ">="  { mkL $ TOperator GEQ }
  "<>"  { mkL $ TOperator NEQ }
  "+"   { mkL $ TOperator Plus }
  "-"   { mkL $ TOperator Minus }
  "*"   { mkL $ TOperator Mult }
  "/"   { mkL $ TOperator Div }
  "&"   { mkL $ TOperator Ampersand }
  "("   { mkL $ TOperator LeftParen }
  ")"   { mkL $ TOperator RightParen }
  "["   { mkL $ TOperator LeftBracket }
  "]"   { mkL $ TOperator RightBracket }
  "{"   { mkL $ TOperator LeftCurly }
  "}"   { mkL $ TOperator RightCurly }
  "@"   { mkL $ TOperator At }
  "?"   { mkL $ TOperator QMark }
  "=>"  { mkL $ TOperator Arrow }
  ".."  { mkL $ TOperator TwoDots }
  "..." { mkL $ TOperator ThreeDots }

{
data Lexeme = L AlexPosn Token String

alexEOF = return (L undefined TEOF "")

mkL :: Token -> AlexInput -> Int -> Alex Lexeme
mkL c (p,_,_,str) len = return (L p c (take len str))


mkInteger :: AlexInput -> Int -> Alex Lexeme
mkInteger (p,_,_,str) len = return (L p (TLiteral $ Integer' value) match)
    where
        match :: String
        match = take len str

        value :: Integer
        value = read match

mkFloat :: AlexInput -> Int -> Alex Lexeme
mkFloat (p,_,_,str) len = return (L p (TLiteral $ Float' value) match)
    where
        match :: String
        match = take len str

        value :: Float
        value = read match

mkString :: AlexInput -> Int -> Alex Lexeme
mkString (p,_,_,str) len = return (L p (TLiteral $ String' (pack match)) match)
    where
        match :: String
        match = take len str

mkQuotedIdentifier :: AlexInput -> Int -> Alex Lexeme
mkQuotedIdentifier (p,_,_,str) len = return (L p (TIdentifier $ QuotedIdentifier (pack match)) match)
    where
        match :: String
        match = take len str

scanner :: String -> Either String [Token]
scanner str = runAlex str $ do
    whileJust (notDone <$> alexMonadScan) $ \(L position token _) -> do
        return token
    where
        notDone :: Lexeme -> Maybe Lexeme
        notDone (L _ TEOF _) = Nothing
        notDone x            = Just x

main = do
    s <- getContents
    print (scanner s)
}
