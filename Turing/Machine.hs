module Turing.Machine (State, Machine(..), Transition(..), fromTuple, tQ, run) where

import Data.List
import Turing.Tape


-- Machine state (STATE) represented by int value
type State = Int


-- Turing Machine (TM) representation 
data Machine = Machine {
    q :: State,
    tape :: Tape,
    transitions :: [[Transition]],
    alphabet :: [Symbol]
} 

-- Pretty print of TM with the transition to apply
instance Show Machine where
    show m@(Machine q tape tr _) = show q ++ " | " ++ show tape ++ "  |  " ++ show transition
        where sym = readTape tape
              transition = choose m


-- Run TM until HALT
run :: Machine -> [Machine]
run m@(Machine q tape tr alp) 
    | transition == Empty = [m]
    | otherwise = m : run (func transition m)
    where transition = choose m


-- Get transition to apply from machine
choose :: Machine -> Transition
choose (Machine q tape tr _) = if q < (length tr) then getTransition (tr !! q) sym else Empty
    where sym = readTape tape


-- Internal Recursive func of choose
getTransition :: [Transition] -> Symbol -> Transition
getTransition [] s = Empty
getTransition (x:xs) s = if (sA x) == s then x else getTransition xs s



-- Type to describe a function to apply to TM
type TransitionFunction = Machine -> Machine 

-- Transition from one state to another where :
--  State to match
--  Symbol to match
--  new State
--  Symbol to write
--  Action
--  transition function (TF)
data Transition = Transition {
    qA :: State,
    sA :: Symbol,
    qF :: State,
    sW :: Symbol,
    act :: Action,
    func :: TransitionFunction
} | Empty 

-- Pretty print transition
instance Show Transition where
    show Empty = "HALT"
    show (Transition qA sA qF sW act _) = "(" ++ show qA ++ ", " ++ show sA ++ ")" ++ " -> " ++ "(" ++ show qF ++ ", " ++ show sW ++ ", " ++ show act ++ ")"

-- Transition equivalence, only used to check if Empty or not
instance Eq Transition where
    Empty == Empty = True
    _ == _ = False
    x /= y = not (x == y)

-- Create a transition from a 5-tuples (for code readability)
fromTuple :: (Int, Symbol, Int, Symbol, Action) -> Transition
fromTuple (qA, sA, qF, sW, act) = Transition{qA=qA, sA=sA, qF=qF, sW=sW, act=act, func = transitionMaker qF sW act}

-- Create transition function
transitionMaker :: State -> Symbol -> Action -> Machine -> Machine
transitionMaker q s act (Machine _ tape tr alp) = (Machine q (move act . writeTape s $ tape) tr alp)




-- hard encoding of unary_sub machine transition
tQ = [
        [   -- tq1
            fromTuple (0, '1', 0, '1', RIGHT),
            fromTuple (0, '-', 0, '-', RIGHT),
            fromTuple (0, '=', 1, '.', LEFT),
            fromTuple (0, '.', 0, '.', RIGHT)
        ],
        [   -- tq2
            fromTuple (1, '1', 2, '=', LEFT),
            fromTuple (1, '-', 4, '.', LEFT)
        ],
        [   -- tq3
            fromTuple (2, '1', 2, '1', LEFT),
            fromTuple (2, '-', 3, '-', LEFT)
        ],
        [   -- tq4
            fromTuple (3, '1', 0, '.', RIGHT),
            fromTuple (3, '.', 3, '.', LEFT)
        ]
    ]

