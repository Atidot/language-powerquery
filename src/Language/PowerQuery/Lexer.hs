{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-missing-signatures #-}
{-# LANGUAGE CPP #-}
{-# LINE 1 "src/Language/PowerQuery/Common/Lexer.x" #-}

{-# LANGUAGE PackageImports #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Language.PowerQuery.Common.Lexer where
import "base" Data.Typeable (Typeable)
import "base" Data.Data (Data)

fdsaf

#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#elif defined(__GLASGOW_HASKELL__)
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Array.Base (unsafeAt)
#else
import Array
#endif
#if __GLASGOW_HASKELL__ >= 503
import System.IO
import System.IO.Unsafe
import Debug.Trace
#else
import IO
import IOExts
#endif
{-# LINE 1 "templates/wrappers.hs" #-}
-- -----------------------------------------------------------------------------
-- Alex wrapper code.
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.






import Data.Word (Word8)
















import Data.Char (ord)
import qualified Data.Bits

-- | Encode a Haskell String to a list of Word8 values, in UTF8 format.
utf8Encode :: Char -> [Word8]
utf8Encode = map fromIntegral . go . ord
 where
  go oc
   | oc <= 0x7f       = [oc]

   | oc <= 0x7ff      = [ 0xc0 + (oc `Data.Bits.shiftR` 6)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]

   | oc <= 0xffff     = [ 0xe0 + (oc `Data.Bits.shiftR` 12)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]
   | otherwise        = [ 0xf0 + (oc `Data.Bits.shiftR` 18)
                        , 0x80 + ((oc `Data.Bits.shiftR` 12) Data.Bits..&. 0x3f)
                        , 0x80 + ((oc `Data.Bits.shiftR` 6) Data.Bits..&. 0x3f)
                        , 0x80 + oc Data.Bits..&. 0x3f
                        ]



type Byte = Word8

-- -----------------------------------------------------------------------------
-- The input type


type AlexInput = (AlexPosn,     -- current position,
                  Char,         -- previous char
                  [Byte],       -- pending bytes on current char
                  String)       -- current input string

ignorePendingBytes :: AlexInput -> AlexInput
ignorePendingBytes (p,c,_ps,s) = (p,c,[],s)

alexInputPrevChar :: AlexInput -> Char
alexInputPrevChar (_p,c,_bs,_s) = c

alexGetByte :: AlexInput -> Maybe (Byte,AlexInput)
alexGetByte (p,c,(b:bs),s) = Just (b,(p,c,bs,s))
alexGetByte (_,_,[],[]) = Nothing
alexGetByte (p,_,[],(c:s))  = let p' = alexMove p c
                                  (b:bs) = utf8Encode c
                              in p' `seq`  Just (b, (p', c, bs, s))





























































-- -----------------------------------------------------------------------------
-- Token positions

-- `Posn' records the location of a token in the input text.  It has three
-- fields: the address (number of chacaters preceding the token), line number
-- and column of a token within the file. `start_pos' gives the position of the
-- start of the file and `eof_pos' a standard encoding for the end of file.
-- `move_pos' calculates the new position after traversing a given character,
-- assuming the usual eight character tab stops.


data AlexPosn = AlexPn !Int !Int !Int
        deriving (Eq,Show)

alexStartPos :: AlexPosn
alexStartPos = AlexPn 0 1 1

alexMove :: AlexPosn -> Char -> AlexPosn
alexMove (AlexPn a l c) '\t' = AlexPn (a+1)  l     (((c+alex_tab_size-1) `div` alex_tab_size)*alex_tab_size+1)
alexMove (AlexPn a l _) '\n' = AlexPn (a+1) (l+1)   1
alexMove (AlexPn a l c) _    = AlexPn (a+1)  l     (c+1)


-- -----------------------------------------------------------------------------
-- Default monad
















































































































-- -----------------------------------------------------------------------------
-- Monad (with ByteString input)







































































































-- -----------------------------------------------------------------------------
-- Basic wrapper

























-- -----------------------------------------------------------------------------
-- Basic wrapper, ByteString version
































-- -----------------------------------------------------------------------------
-- Posn wrapper

-- Adds text positions to the basic model.


--alexScanTokens :: String -> [token]
alexScanTokens str0 = go (alexStartPos,'\n',[],str0)
  where go inp__@(pos,_,_,str) =
          case alexScan inp__ 0 of
                AlexEOF -> []
                AlexError ((AlexPn _ line column),_,_,_) -> error $ "lexical error at line " ++ (show line) ++ ", column " ++ (show column)
                AlexSkip  inp__' _ln     -> go inp__'
                AlexToken inp__' len act -> act pos (take len str) : go inp__'



-- -----------------------------------------------------------------------------
-- Posn wrapper, ByteString version














-- -----------------------------------------------------------------------------
-- GScan wrapper

-- For compatibility with previous versions of Alex, and because we can.














alex_tab_size :: Int
alex_tab_size = 8
alex_base :: Array Int Int
alex_base = listArray (0 :: Int, 151)
  [ -8
  , -104
  , -94
  , -93
  , -88
  , -91
  , -89
  , -103
  , -86
  , -108
  , -85
  , -84
  , -82
  , -101
  , -92
  , -80
  , -79
  , -74
  , -87
  , -71
  , -73
  , -75
  , -70
  , -72
  , -67
  , -56
  , -55
  , -78
  , -64
  , -68
  , -53
  , -60
  , -47
  , -44
  , -46
  , -45
  , -39
  , -35
  , -42
  , -43
  , -41
  , -40
  , -38
  , -31
  , -28
  , -49
  , -26
  , -33
  , -22
  , -23
  , -27
  , -20
  , -25
  , -17
  , -18
  , -24
  , -16
  , -15
  , -19
  , -5
  , -37
  , 5
  , -10
  , -12
  , -11
  , 1
  , -6
  , -7
  , -2
  , 16
  , -1
  , 10
  , 6
  , 11
  , 12
  , 7
  , 14
  , 9
  , 13
  , 17
  , 24
  , 18
  , 44
  , 30
  , 23
  , 25
  , 26
  , 37
  , 29
  , 27
  , 28
  , 31
  , 38
  , 32
  , 45
  , 42
  , 145
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 34
  , -30
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 89
  , 98
  , 0
  , 91
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 115
  , 0
  ]

alex_table :: Array Int Int
alex_table = listArray (0 :: Int, 400)
  [ 0
  , 96
  , 96
  , 96
  , 96
  , 96
  , 95
  , 100
  , 102
  , 107
  , 109
  , 98
  , 114
  , 117
  , 6
  , 116
  , 118
  , 119
  , 115
  , 120
  , 122
  , 126
  , 127
  , 124
  , 96
  , 94
  , 91
  , 69
  , 2
  , 84
  , 140
  , 11
  , 141
  , 142
  , 138
  , 136
  , 128
  , 137
  , 82
  , 139
  , 93
  , 90
  , 3
  , 5
  , 88
  , 87
  , 86
  , 85
  , 83
  , 16
  , 17
  , 129
  , 131
  , 130
  , 133
  , 148
  , 147
  , 81
  , 78
  , 23
  , 24
  , 18
  , 77
  , 27
  , 28
  , 75
  , 67
  , 72
  , 76
  , 71
  , 34
  , 70
  , 42
  , 74
  , 31
  , 68
  , 66
  , 64
  , 65
  , 47
  , 63
  , 60
  , 58
  , 143
  , 62
  , 144
  , 57
  , 52
  , 53
  , 1
  , 150
  , 73
  , 39
  , 32
  , 44
  , 51
  , 54
  , 92
  , 41
  , 48
  , 21
  , 33
  , 22
  , 89
  , 45
  , 40
  , 49
  , 61
  , 80
  , 46
  , 35
  , 36
  , 30
  , 25
  , 50
  , 145
  , 37
  , 146
  , 20
  , 15
  , 19
  , 55
  , 14
  , 13
  , 12
  , 10
  , 29
  , 9
  , 26
  , 8
  , 125
  , 59
  , 43
  , 123
  , 4
  , 121
  , 113
  , 112
  , 7
  , 111
  , 103
  , 110
  , 97
  , 56
  , 108
  , 79
  , 101
  , 106
  , 104
  , 99
  , 38
  , 149
  , 134
  , 105
  , 96
  , 96
  , 96
  , 96
  , 96
  , 132
  , 135
  , 151
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 96
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  , 0
  ]

alex_check :: Array Int Int
alex_check = listArray (0 :: Int, 400)
  [ -1
  , 9
  , 10
  , 11
  , 12
  , 13
  , 110
  , 101
  , 101
  , 97
  , 101
  , 115
  , 101
  , 121
  , 117
  , 101
  , 101
  , 101
  , 121
  , 101
  , 121
  , 101
  , 101
  , 115
  , 32
  , 99
  , 101
  , 35
  , 115
  , 97
  , 38
  , 109
  , 40
  , 41
  , 42
  , 43
  , 44
  , 45
  , 46
  , 47
  , 111
  , 111
  , 115
  , 115
  , 111
  , 101
  , 101
  , 111
  , 101
  , 109
  , 97
  , 59
  , 60
  , 61
  , 62
  , 63
  , 64
  , 101
  , 97
  , 105
  , 105
  , 108
  , 97
  , 105
  , 105
  , 105
  , 97
  , 114
  , 111
  , 97
  , 119
  , 97
  , 105
  , 111
  , 105
  , 97
  , 99
  , 97
  , 105
  , 116
  , 105
  , 99
  , 117
  , 91
  , 101
  , 93
  , 110
  , 102
  , 104
  , 97
  , 46
  , 98
  , 122
  , 101
  , 102
  , 114
  , 101
  , 105
  , 110
  , 104
  , 108
  , 109
  , 110
  , 111
  , 114
  , 116
  , 101
  , 115
  , 116
  , 104
  , 116
  , 110
  , 114
  , 114
  , 98
  , 123
  , 100
  , 125
  , 108
  , 108
  , 114
  , 105
  , 110
  , 116
  , 110
  , 116
  , 110
  , 114
  , 104
  , 112
  , 100
  , 115
  , 116
  , 110
  , 116
  , 110
  , 110
  , 100
  , 114
  , 110
  , 102
  , 114
  , 100
  , 116
  , 116
  , 121
  , 114
  , 116
  , 110
  , 104
  , 116
  , 62
  , 61
  , 115
  , 9
  , 10
  , 11
  , 12
  , 13
  , 61
  , 62
  , 46
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , 32
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  ]

alex_deflt :: Array Int Int
alex_deflt = listArray (0 :: Int, 151)
  [ -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  , -1
  ]

alex_accept = listArray (0 :: Int, 151)
  [ AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccNone
  , AlexAccSkip
  , AlexAcc 54
  , AlexAcc 53
  , AlexAcc 52
  , AlexAcc 51
  , AlexAcc 50
  , AlexAcc 49
  , AlexAcc 48
  , AlexAcc 47
  , AlexAcc 46
  , AlexAcc 45
  , AlexAcc 44
  , AlexAcc 43
  , AlexAcc 42
  , AlexAcc 41
  , AlexAcc 40
  , AlexAcc 39
  , AlexAcc 38
  , AlexAcc 37
  , AlexAcc 36
  , AlexAcc 35
  , AlexAcc 34
  , AlexAcc 33
  , AlexAcc 32
  , AlexAcc 31
  , AlexAcc 30
  , AlexAcc 29
  , AlexAcc 28
  , AlexAcc 27
  , AlexAcc 26
  , AlexAcc 25
  , AlexAcc 24
  , AlexAcc 23
  , AlexAcc 22
  , AlexAcc 21
  , AlexAcc 20
  , AlexAcc 19
  , AlexAcc 18
  , AlexAcc 17
  , AlexAcc 16
  , AlexAcc 15
  , AlexAcc 14
  , AlexAcc 13
  , AlexAcc 12
  , AlexAcc 11
  , AlexAcc 10
  , AlexAcc 9
  , AlexAcc 8
  , AlexAcc 7
  , AlexAcc 6
  , AlexAcc 5
  , AlexAcc 4
  , AlexAcc 3
  , AlexAcc 2
  , AlexAcc 1
  , AlexAcc 0
  ]

alex_actions = array (0 :: Int, 55)
  [ (54,alex_action_1)
  , (53,alex_action_2)
  , (52,alex_action_3)
  , (51,alex_action_4)
  , (50,alex_action_5)
  , (49,alex_action_6)
  , (48,alex_action_7)
  , (47,alex_action_8)
  , (46,alex_action_9)
  , (45,alex_action_10)
  , (44,alex_action_11)
  , (43,alex_action_12)
  , (42,alex_action_13)
  , (41,alex_action_14)
  , (40,alex_action_15)
  , (39,alex_action_16)
  , (38,alex_action_17)
  , (37,alex_action_18)
  , (36,alex_action_19)
  , (35,alex_action_20)
  , (34,alex_action_21)
  , (33,alex_action_22)
  , (32,alex_action_23)
  , (31,alex_action_24)
  , (30,alex_action_25)
  , (29,alex_action_26)
  , (28,alex_action_27)
  , (27,alex_action_28)
  , (26,alex_action_29)
  , (25,alex_action_30)
  , (24,alex_action_31)
  , (23,alex_action_32)
  , (22,alex_action_33)
  , (21,alex_action_34)
  , (20,alex_action_35)
  , (19,alex_action_36)
  , (18,alex_action_37)
  , (17,alex_action_38)
  , (16,alex_action_39)
  , (15,alex_action_40)
  , (14,alex_action_41)
  , (13,alex_action_42)
  , (12,alex_action_43)
  , (11,alex_action_44)
  , (10,alex_action_45)
  , (9,alex_action_46)
  , (8,alex_action_47)
  , (7,alex_action_48)
  , (6,alex_action_49)
  , (5,alex_action_50)
  , (4,alex_action_51)
  , (3,alex_action_52)
  , (2,alex_action_53)
  , (1,alex_action_54)
  , (0,alex_action_55)
  ]

{-# LINE 77 "src/Language/PowerQuery/Common/Lexer.x" #-}

data Token
    = TAnd
    | TAs
    | TEech
    | TElse
    | TError
    | TFalse
    | TIf
    | TIn
    | TIs
    | TLet
    | TMeta
    | TNot
    | TOtherwise
    | TOr
    | TSection
    | TShared
    | TThen
    | TTrue
    | TTry
    | TType
    | TBinary
    | TDate
    | TDateTime
    | TDateTimezone
    | TDuration
    | TInfinity
    | TNan
    | TSections
    | THShared
    | TTable
    | TTime

    | TComma
    | TSemicolon
    | TEquals
    | TLT
    | TLEQ
    | TGT
    | TGEQ
    | TNEQ
    | TPlus
    | TMinus
    | TMul
    | TDiv
    | TAmp
    | TAt
    | TQMark
    | TArrow
    | TTwoDots
    | TThreeDots
    | TLeftParen
    | TRightParen
    | TLeftBrace
    | TRightBrace
    | TLeftBrack
    | TRightBrack
    deriving (Eq, Show, Data, Typeable)

main = do
    s <- getContents
    print (alexScanTokens s)

alex_action_1 =  \s -> TAnd 
alex_action_2 =  \s -> TAs 
alex_action_3 =  \s -> TEech 
alex_action_4 =  \s -> TElse 
alex_action_5 =  \s -> TError 
alex_action_6 =  \s -> TFalse 
alex_action_7 =  \s -> TIf 
alex_action_8 =  \s -> TIn 
alex_action_9 =  \s -> TIs 
alex_action_10 =  \s -> TLet 
alex_action_11 =  \s -> TMeta 
alex_action_12 =  \s -> TNot 
alex_action_13 =  \s -> TOtherwise 
alex_action_14 =  \s -> TOr 
alex_action_15 =  \s -> TSection 
alex_action_16 =  \s -> TShared 
alex_action_17 =  \s -> TThen 
alex_action_18 =  \s -> TTrue 
alex_action_19 =  \s -> TTry 
alex_action_20 =  \s -> TType 
alex_action_21 =  \s -> TBinary 
alex_action_22 =  \s -> TDate 
alex_action_23 =  \s -> TDateTime 
alex_action_24 =  \s -> TDateTimezone 
alex_action_25 =  \s -> TDuration 
alex_action_26 =  \s -> TInfinity 
alex_action_27 =  \s -> TNan 
alex_action_28 =  \s -> TSections 
alex_action_29 =  \s -> THShared 
alex_action_30 =  \s -> TTable 
alex_action_31 =  \s -> TTime 
alex_action_32 =  \s -> TComma 
alex_action_33 =  \s -> TSemicolon 
alex_action_34 =  \s -> TEquals 
alex_action_35 =  \s -> TLT 
alex_action_36 =  \s -> TLEQ 
alex_action_37 =  \s -> TGT 
alex_action_38 =  \s -> TGEQ 
alex_action_39 =  \s -> TNEQ 
alex_action_40 =  \s -> TPlus 
alex_action_41 =  \s -> TMinus 
alex_action_42 =  \s -> TMul 
alex_action_43 =  \s -> TDiv 
alex_action_44 =  \s -> TAmp 
alex_action_45 =  \s -> TLeftParen 
alex_action_46 =  \s -> TRightParen 
alex_action_47 =  \s -> TLeftBrack 
alex_action_48 =  \s -> TRightBrack 
alex_action_49 =  \s -> TLeftBrace 
alex_action_50 =  \s -> TRightBrace 
alex_action_51 =  \s -> TAt 
alex_action_52 =  \s -> TQMark 
alex_action_53 =  \s -> TArrow 
alex_action_54 =  \s -> TTwoDots 
alex_action_55 =  \s -> TThreeDots 
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine































































alexIndexInt16OffAddr arr off = arr ! off




















alexIndexInt32OffAddr arr off = arr ! off











quickIndex arr i = arr ! i


-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input__ (sc)
  = alexScanUser undefined input__ (sc)

alexScanUser user__ input__ (sc)
  = case alex_scan_tkn user__ input__ (0) input__ sc AlexNone of
  (AlexNone, input__') ->
    case alexGetByte input__ of
      Nothing ->

                                   trace ("End of input.") $

                                   AlexEOF
      Just _ ->

                                   trace ("Error.") $

                                   AlexError input__'

  (AlexLastSkip input__'' len, _) ->

    trace ("Skipping.") $

    AlexSkip input__'' len

  (AlexLastAcc k input__''' len, _) ->

    trace ("Accept.") $

    AlexToken input__''' len (alex_actions ! k)


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user__ orig_input len input__ s last_acc =
  input__ `seq` -- strict in the input
  let
  new_acc = (check_accs (alex_accept `quickIndex` (s)))
  in
  new_acc `seq`
  case alexGetByte input__ of
     Nothing -> (new_acc, input__)
     Just (c, new_input) ->

      trace ("State: " ++ show (s) ++ ", char: " ++ show c) $

      case fromIntegral c of { (ord_c) ->
        let
                base   = alexIndexInt32OffAddr alex_base s
                offset = (base + ord_c)
                check  = alexIndexInt16OffAddr alex_check offset

                new_s = if (offset >= (0)) && (check == ord_c)
                          then alexIndexInt16OffAddr alex_table offset
                          else alexIndexInt16OffAddr alex_deflt s
        in
        case new_s of
            (-1) -> (new_acc, input__)
                -- on an error, we want to keep the input *before* the
                -- character that failed, not after.
            _ -> alex_scan_tkn user__ orig_input (if c < 0x80 || c >= 0xC0 then (len + (1)) else len)
                                                -- note that the length is increased ONLY if this is the 1st byte in a char encoding)
                        new_input new_s new_acc
      }
  where
        check_accs (AlexAccNone) = last_acc
        check_accs (AlexAcc a  ) = AlexLastAcc a input__ (len)
        check_accs (AlexAccSkip) = AlexLastSkip  input__ (len)

        check_accs (AlexAccPred a predx rest)
           | predx user__ orig_input (len) input__
           = AlexLastAcc a input__ (len)
           | otherwise
           = check_accs rest
        check_accs (AlexAccSkipPred predx rest)
           | predx user__ orig_input (len) input__
           = AlexLastSkip input__ (len)
           | otherwise
           = check_accs rest


data AlexLastAcc
  = AlexNone
  | AlexLastAcc !Int !AlexInput !Int
  | AlexLastSkip     !AlexInput !Int

data AlexAcc user
  = AlexAccNone
  | AlexAcc Int
  | AlexAccSkip

  | AlexAccPred Int (AlexAccPred user) (AlexAcc user)
  | AlexAccSkipPred (AlexAccPred user) (AlexAcc user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user__ in1 len in2
  = p1 user__ in1 len in2 && p2 user__ in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _
alexPrevCharIs c _ input__ _ _ = c == alexInputPrevChar input__

alexPrevCharMatches f _ input__ _ _ = f (alexInputPrevChar input__)

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _
alexPrevCharIsOneOf arr _ input__ _ _ = arr ! alexInputPrevChar input__

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (sc) user__ _ _ input__ =
     case alex_scan_tkn user__ input__ (0) input__ sc AlexNone of
          (AlexNone, _) -> False
          _ -> True
        -- TODO: there's no need to find the longest
        -- match when checking the right context, just
        -- the first match will do.

