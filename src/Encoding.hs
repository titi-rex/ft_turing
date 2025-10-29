module Encoding
  ( Symbol,
    PrettySymbol,
    defaultBlank,
    defaultPrettyBlank,
    symbolToPretty,
    prettyToSymbol,
    symbolToPrettyString,
    prettyToSymbolString,
    unaryEncodeNumber,
    Action (..),
    unaryEncodeAction,
  )
where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

-- Symbol (S) who can be read/write from the tape
-- For abstraction, they're internaly stored as int, starting to 1
type Symbol = Int

-- Default Blank Symbol (BLANK) used to fill Tape
defaultBlank :: Symbol
defaultBlank = 3

-- Human readable version of Symbol (PS) used to display Symbol
type PrettySymbol = Char

-- Default Pretty Blank Symbol
defaultPrettyBlank :: PrettySymbol
defaultPrettyBlank = '.'

-- Possible action on the tape
data Action = LEFT | RIGHT | HALT deriving (Show, Eq)

-- Symbol <-> PrettySymbol conversion function (use a PS alphabet to map S to PS)

-- Encode Symbol to PrettySymbol
-- example: '1' -> 1, '-' -> 2, '.' -> 3
prettyToSymbol :: [PrettySymbol] -> PrettySymbol -> Symbol
prettyToSymbol alp psym = (fromMaybe (2) $ elemIndex psym alp) + 1

-- Decode PrettySymbol to Symbol
-- example: 1 -> '1', 2 -> '-', 3 -> '.'
symbolToPretty :: [PrettySymbol] -> Symbol -> PrettySymbol
symbolToPretty alp sym
  | exist == True = alp !! symShifted
  | otherwise = defaultPrettyBlank
  where
    symShifted = sym - 1
    exist = symShifted < (length alp)

-- Decode a [Symbol] to a String
symbolToPrettyString :: [PrettySymbol] -> [Symbol] -> String
symbolToPrettyString alp l = map (symbolToPretty alp) l

-- Encode a String into its [Symbol] representation
prettyToSymbolString :: [PrettySymbol] -> String -> [Symbol]
prettyToSymbolString alp l = map (prettyToSymbol alp) l

-- Unary encoding function

-- Return an unary encoding of an Int
unaryEncodeNumber :: Int -> String
unaryEncodeNumber 0 = ""
unaryEncodeNumber q = "0" ++ unaryEncodeNumber (q - 1)

-- Return an unary encoding of an Action
unaryEncodeAction :: Action -> String
unaryEncodeAction LEFT = "0"
unaryEncodeAction RIGHT = "00"

{--
other index can be arbitrary

state:
q1: initial ->  0
(q2: accept  ->  00
q3: reject  ->  000)
q4: halting ->  0000  -> no accept/reject ->  q2  -> 00

symbol:
s1: 0     ->  0
s2: 1     ->  00
s3: blank ->  000

direction:
left  ->  0
right ->  00
hold  ->  000

--}
