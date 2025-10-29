module Tape
  ( Tape (..),
    Action (..),
    defaultBlank,
    fromEncodedString,
    move,
    writeTape,
    readTape,
  )
where

import Encoding

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
  show (Tape hl cs tl alp) = ['['] ++ prettyHl ++ ['<', prettyCs, '>'] ++ prettyTl ++ [']']
    where
      prettyHl = symbolToPrettyString alp hl
      prettyCs = symbolToPretty alp cs
      prettyTl = symbolToPrettyString alp tl

-- Construt a Tape from a Symbol List
-- CS is set at first element
fromEncodedString :: [PrettySymbol] -> [Symbol] -> Tape
fromEncodedString alp [] = (Tape [] defaultBlank [] alp)
fromEncodedString alp (x : xs) = (Tape [] x xs alp)

-- Write a new Symbol to the tape in place of CS
writeTape :: Symbol -> Tape -> Tape
writeTape s t@(Tape _ cs _ _) = t {cs = s}

-- Read CS
readTape :: Tape -> Symbol
readTape (Tape _ cs _ _) = cs

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
