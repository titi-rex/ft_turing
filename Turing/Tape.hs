module Turing.Tape (Symbol, Tape(..), Taper, Action(..), fromString, move, writeTape, readTape) where


-- Symbol who can be read/write from the tape
-- For simplicity, we use Char
type Symbol = Char


-- Blank Symbol (BLANK) used to fill Tape
blank = '.'


-- Tape data, composed of :
--  a head List (HL)
--  the current Symbol (CS) (head of turing machine)
--  a tail List (TL)
data Tape = Tape {
    hl :: [Symbol],
    cs :: Symbol,
    tl :: [Symbol]
} 


type Taper = (Tape -> Tape)


-- For now, show Tape with hardcoded separator
instance Show Tape where
    show (Tape hl cs tl) = ['['] ++ hl ++ ['<', cs, '>'] ++ tl ++ [']']


-- Construt a Tape from a String input
-- CS is set at first element
fromString :: String -> Tape
fromString [] = (Tape [] blank [])
fromString (x:xs) = (Tape [] x xs)


-- Possible action on the tape
data Action = LEFT | RIGHT | HALT deriving (Show, Eq)


-- Move CS to the left
-- Fill Tape with BLANK if needed
moveLeft :: Tape -> Tape
moveLeft (Tape [] cs tl) = Tape [] blank (cs:tl)
moveLeft (Tape hl cs tl) = Tape (init hl) (last hl) (cs:tl)


-- Move CS to the right
-- Fill Tape with BLANK if needed
moveRight :: Tape -> Tape
moveRight (Tape hl cs []) = Tape (hl++[cs]) blank []
moveRight (Tape hl cs (x:xs)) = Tape (hl ++ [cs]) x xs


-- Move CS based on Action supplied
move :: Action -> Tape -> Tape
move LEFT tape = moveLeft tape
move RIGHT tape = moveRight tape
move HALT tape = tape


-- Write a new Symbol to the tape in place of CS
writeTape :: Symbol -> Tape -> Tape
writeTape s (Tape hl cs tl) = Tape hl s tl


-- Read CS
readTape :: Tape -> Symbol
readTape (Tape hl cs tl) = cs

