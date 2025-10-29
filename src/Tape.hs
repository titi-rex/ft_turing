module Tape
  ( Symbol,
    PrettySymbol,
    Tape (..),
    Action (..),
    defaultBlank,
    fromEncodedString,
    toDecodedString,
    toEncodedString,
    move,
    writeTape,
    readTape,
    symbolToPretty,
    prettyToSymbol,
  )
where

import Data.List (elemIndex)
import Data.Maybe (fromMaybe)

-- Symbol who can be read/write from the tape
-- For simplicity, we use Char
type Symbol = Int

-- Type used to display Symbol for human
type PrettySymbol = Char

-- Blank Symbol (BLANK) used to fill Tape
defaultBlank = 3

defaultPrettyBlank = '.'

-- TODO: change tape to use Data.List.setAt nad one list ?
-- Tape data, composed of :
--  a head List (HL)
--  the current Symbol (CS) (head of turing machine)
--  a tail List (TL)
data Tape = Tape
  { hl :: [Symbol],
    cs :: Symbol,
    tl :: [Symbol],
    alphabet :: [PrettySymbol]
  }

-- For now, show Tape with hardcoded separator
instance Show Tape where
  show (Tape hl cs tl alp) = ['['] ++ show prettyHl ++ ['<'] ++ show prettyCs ++ ['>'] ++ show prettyTl ++ [']']
    where
      prettyHl = toDecodedString alp hl
      prettyCs = symbolToPretty alp cs
      prettyTl = toDecodedString alp tl

-- Construt a Tape from a Symbol List
-- CS is set at first element
fromEncodedString :: [PrettySymbol] -> [Symbol] -> Tape
fromEncodedString alp [] = (Tape [] defaultBlank [] alp)
fromEncodedString alp (x : xs) = (Tape [] x xs alp)

-- Decode a [Symbol] to a String using a PrettySymbol alphabet
toDecodedString :: [PrettySymbol] -> [Symbol] -> String
toDecodedString alp l = map (symbolToPretty alp) l

-- Encode a String into its [Symbol] representation using a PrettySymbol alphabet
toEncodedString :: [PrettySymbol] -> String -> [Symbol]
toEncodedString alp l = map (prettyToSymbol alp) l

-- Encode Symbol to PrettySymbol using Alphabet
-- example: '1' -> 1, '-' -> 2, '.' -> 3
prettyToSymbol :: [PrettySymbol] -> PrettySymbol -> Symbol
prettyToSymbol alp psym = fromMaybe (2) $ elemIndex psym alp

-- Decode PrettySymbol to Symbol using Alphabet
-- example: 1 -> '1', 2 -> '-', 3 -> '.'
symbolToPretty :: [PrettySymbol] -> Symbol -> PrettySymbol
symbolToPretty alp sym
  | exist == True = alp !! sym
  | otherwise = defaultPrettyBlank
  where
    exist = sym < (length alp)

-- Write a new Symbol to the tape in place of CS
writeTape :: Symbol -> Tape -> Tape
writeTape s t@(Tape _ cs _ _) = t {cs = s}

-- Read CS
readTape :: Tape -> Symbol
readTape (Tape _ cs _ _) = cs

-- Possible action on the tape
data Action = LEFT | RIGHT | HALT deriving (Show, Eq)

-- Move CS based on Action supplied
move :: Action -> Tape -> Tape
move LEFT tape = moveLeft tape
move RIGHT tape = moveRight tape
move HALT tape = tape

-- Move CS to the left
-- Fill Tape with BLANK if needed
moveLeft :: Tape -> Tape
moveLeft (Tape [] cs tl alp) = Tape [] defaultBlank (cs : tl) alp
moveLeft (Tape hl cs tl alp) = Tape (init hl) (last hl) (cs : tl) alp

-- Move CS to the right
-- Fill Tape with BLANK if needed
moveRight :: Tape -> Tape
moveRight (Tape hl cs [] alp) = Tape (hl ++ [cs]) defaultBlank [] alp
moveRight t@(Tape hl cs (x : xs) alp) = Tape (hl ++ [cs]) x xs alp
