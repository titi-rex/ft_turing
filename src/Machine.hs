module Machine
  ( State,
    Machine (..),
    Transition (..),
    fromTuple,
    run,
    choose,
    prettyTransition,
  )
where

import Data.List
import Debug
import Print (padR)
import Tape

-- Machine state (STATE) represented by int value
type State = Int

-- Turing Machine (TM) representation
data Machine = Machine
  { q :: State,
    tape :: Tape,
    transitions :: [[Transition]],
    alphabet :: [Symbol],
    prettyStates :: [String]
  }

-- Pretty print of TM with the transition to apply
instance Show Machine where
  show m@(Machine q tape tr _ prettyStates) = (padR 10 prettyQ) ++ " |  " ++ show tape ++ "  |  " ++ transition
    where
      prettyQ = prettyStates !! q
      transition = (prettyTransition prettyStates) . choose $ m

debugMachine :: Machine -> String
debugMachine m@(Machine q tape tr _ _) = show (q + 1) ++ " |  " ++ show tape ++ "  |  " ++ show transition
  where
    transition = choose m

-- Run TM until HALT
run :: Machine -> [Machine]
run m
  | transition == Empty = [m]
  | transition == Final = [m]
  | otherwise = m : run (func transition m)
  where
    transition = choose m

-- Get transition to apply from machine
choose :: Machine -> Transition
choose (Machine q tape tr _ _) = if q < (length tr) then getTransition (tr !! q) sym else Final
  where
    sym = readTape tape

-- Internal Recursive func of choose
getTransition :: [Transition] -> Symbol -> Transition
getTransition [] s = Empty
getTransition (x : xs) s = if (sA x) == s then x else getTransition xs s

-- Type to describe a function to apply to TM
type TransitionFunction = Machine -> Machine

-- Transition from one state to another where :
--  State to match
--  Symbol to match
--  new State
--  Symbol to write
--  Action
--  transition function (TF)
data Transition
  = Transition
      { qA :: State,
        sA :: Symbol,
        qF :: State,
        sW :: Symbol,
        act :: Action,
        func :: TransitionFunction
      }
  | Empty
  | Final

-- Pretty print transition
instance Show Transition where
  show Final = "FINAL"
  show Empty = "BLOCKED"
  show (Transition qA sA qF sW act _) = "(" ++ show qA ++ ", " ++ show sA ++ ")" ++ " -> " ++ "(" ++ show qF ++ ", " ++ show sW ++ ", " ++ show act ++ ")"

-- Transition equivalence, only used to check if Empty or not
instance Eq Transition where
  Empty == Empty = True
  Final == Final = True
  _ == _ = False
  x /= y = not (x == y)

prettyTransition :: [String] -> Transition -> String
prettyTransition states Final = "Final"
prettyTransition states Empty = "Blocked  -> no transition possible"
prettyTransition states (Transition qA sA qF sW act _) = (padR 17 $ "(" ++ prettyQA ++ ", " ++ show sA ++ ")") ++ " ->  " ++ "(" ++ prettyQF ++ ", " ++ show sW ++ ", " ++ show act ++ ")"
  where
    prettyQA = states !! (qA - 1)
    prettyQF = states !! (qF - 1)

-- Create a transition from a 5-tuples (for code readability)
fromTuple :: (Int, Symbol, Int, Symbol, Action) -> Transition
fromTuple (qA, sA, qF, sW, act) = Transition {qA = qA, sA = sA, qF = qF, sW = sW, act = act, func = transitionMaker qF sW act}

-- Create transition function
transitionMaker :: State -> Symbol -> Action -> Machine -> Machine
transitionMaker q s act m@(Machine _ tape tr _ _) = m {q = (q - 1), tape = move act . writeTape s $ tape}
