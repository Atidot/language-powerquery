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
import               Language.PowerQuery.AST.Token
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
  <0> "//".*   { mkL CommentT }

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
  <0> null                   { mkL $ LiteralT NullL }

  --==========
  -- 12.1.6 - quoted-identifer
  <0> \# \" @string* \"       { mkQuotedIdentifier }


  --==========
  -- 12.1.7 - Keywords and predefined identifiers
  "and"            { mkL $ KeywordT AndK }
  "as"             { mkL $ KeywordT AsK }
  "each"           { mkL $ KeywordT EachK }
  "else"           { mkL $ KeywordT ElseK }
  "error"          { mkL $ KeywordT ErrorK }
  "false"          { mkL $ KeywordT FalseK }
  "if"             { mkL $ KeywordT IfK }
  "in"             { mkL $ KeywordT InK }
  "is"             { mkL $ KeywordT IsK }
  "let"            { mkL $ KeywordT LetK }
  "meta"           { mkL $ KeywordT MetaK }
  "not"            { mkL $ KeywordT NotK }
  "otherwise"      { mkL $ KeywordT OtherwiseK }
  "or"             { mkL $ KeywordT OrK }
  "section"        { mkL $ KeywordT SectionK }
  "shared"         { mkL $ KeywordT SharedK }
  "then"           { mkL $ KeywordT ThenK }
  "true"           { mkL $ KeywordT TrueK }
  "try"            { mkL $ KeywordT TryK }
  "type"           { mkL $ KeywordT TypeK }
  "#binary"        { mkL $ KeywordT H_BinaryK }
  "#date"          { mkL $ KeywordT H_DateK }
  "#datetime"      { mkL $ KeywordT H_DateTimeK }
  "#datetimezone"  { mkL $ KeywordT H_DateTimezoneK }
  "#duration"      { mkL $ KeywordT H_DurationK }
  "#infinity"      { mkL $ KeywordT H_InfinityK }
  "#nan"           { mkL $ KeywordT H_NanK }
  "#sections"      { mkL $ KeywordT H_SectionsK }
  "#shared"        { mkL $ KeywordT H_SharedK }
  "#table"         { mkL $ KeywordT H_TableK }
  "#time"          { mkL $ KeywordT H_TimeK }


  --==========
  -- 12.1.8 - Operators and punctuators
  ","   { mkL $ OperatorT CommaO }
  ";"   { mkL $ OperatorT SemiColonO }
  "="   { mkL $ OperatorT EqualO }
  "<"   { mkL $ OperatorT LT_O }
  "<="  { mkL $ OperatorT LEQ_O }
  ">"   { mkL $ OperatorT GT_O }
  ">="  { mkL $ OperatorT GEQ_O }
  "<>"  { mkL $ OperatorT NEQ_O }
  "+"   { mkL $ OperatorT PlusO }
  "-"   { mkL $ OperatorT MinusO }
  "*"   { mkL $ OperatorT MultO }
  "/"   { mkL $ OperatorT DivO }
  "&"   { mkL $ OperatorT AmpersandO }
  "("   { mkL $ OperatorT LeftParenO }
  ")"   { mkL $ OperatorT RightParenO }
  "["   { mkL $ OperatorT LeftBracketO }
  "]"   { mkL $ OperatorT RightBracketO }
  "{"   { mkL $ OperatorT LeftCurlyO }
  "}"   { mkL $ OperatorT RightCurlyO }
  "@"   { mkL $ OperatorT AtO }
  "?"   { mkL $ OperatorT QMarkO }
  "=>"  { mkL $ OperatorT ArrowO }
  ".."  { mkL $ OperatorT TwoDotsO }
  "..." { mkL $ OperatorT ThreeDotsO }

{
data Lexeme = L AlexPosn Token String

eOF = (L undefined EOF_T "")
alexEOF = return eOF

mkL :: Token -> AlexInput -> Int -> Alex Lexeme
mkL c (p,_,_,str) len = return (L p c (take len str))


mkInteger :: AlexInput -> Int -> Alex Lexeme
mkInteger (p,_,_,str) len = return (L p (LiteralT $ IntegerL value) match)
    where
        match :: String
        match = take len str

        value :: Integer
        value = read match

mkFloat :: AlexInput -> Int -> Alex Lexeme
mkFloat (p,_,_,str) len = return (L p (LiteralT $ FloatL value) match)
    where
        match :: String
        match = take len str

        value :: Float
        value = read match

mkString :: AlexInput -> Int -> Alex Lexeme
mkString (p,_,_,str) len = return (L p (LiteralT $ StringL (pack match)) match)
    where
        match :: String
        match = take len str

mkQuotedIdentifier :: AlexInput -> Int -> Alex Lexeme
mkQuotedIdentifier (p,_,_,str) len = return (L p (IdentifierT $ QuotedI (pack match)) match)
    where
        match :: String
        match = take len str

lexer :: String -> Either String [Token]
lexer str = runAlex str $ do
    whileJust (notDone <$> alexMonadScan) $ \(L position token _) -> do
        return token
    where
        notDone :: Lexeme -> Maybe Lexeme
        notDone (L _ EOF_T _) = Nothing
        notDone x            = Just x

main' = do
    s <- getContents
    print (lexer s)
}
