module Tape (Symbol, Tape (..), Action (..), defaultBlank, fromString, move, writeTape, readTape) where

-- Symbol who can be read/write from the tape
-- For simplicity, we use Char
type Symbol = Char

-- Blank Symbol (BLANK) used to fill Tape
defaultBlank = '.'

-- Tape data, composed of :
--  a head List (HL)
--  the current Symbol (CS) (head of turing machine)
--  a tail List (TL)
data Tape = Tape
  { hl :: [Symbol],
    cs :: Symbol,
    tl :: [Symbol],
    blank :: Symbol
  }

-- For now, show Tape with hardcoded separator
instance Show Tape where
  show (Tape hl cs tl _) = ['['] ++ hl ++ ['<', cs, '>'] ++ tl ++ [']']

-- Construt a Tape from a String input
-- CS is set at first element
fromString :: Symbol -> String -> Tape
fromString b [] = (Tape [] b [] b)
fromString b (x : xs) = (Tape [] x xs b)

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
moveLeft (Tape [] cs tl blank) = Tape [] blank (cs : tl) blank
moveLeft (Tape hl cs tl blank) = Tape (init hl) (last hl) (cs : tl) blank

-- Move CS to the right
-- Fill Tape with BLANK if needed
moveRight :: Tape -> Tape
moveRight (Tape hl cs [] blank) = Tape (hl ++ [cs]) blank [] blank
moveRight t@(Tape hl cs (x : xs) blank) = Tape (hl ++ [cs]) x xs blank
