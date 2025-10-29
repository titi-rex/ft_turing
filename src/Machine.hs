module Machine
  ( State,
    Machine (..),
    Transition (..),
    transitionFromTupleEncoded,
    transitionFromTuple,
    run,
    encode,
  )
where

import Data.List
import Encoding
import Tape

-- Machine state (STATE) represented by int value
type State = Int

-- Turing Machine (TM) representation
data Machine = Machine
  { q :: State,
    tape :: Tape,
    transitions :: [[Transition]]
  }

-- Pretty print of TM with the transition to apply
instance Show Machine where
  show m@(Machine q tape tr) = show (q + 1) ++ " | " ++ show tape ++ "  |  " ++ show transition
    where
      transition = choose m

-- Return the machine unary encoding
encode :: Machine -> String
encode (Machine _ _ t) = concat . intersperse "11" . map encode_transition . concat $ t

-- Return the transition unary encoding
encode_transition :: Transition -> String
encode_transition (Transition q r f w a _) = en q ++ "1" ++ en r ++ "1" ++ en f ++ "1" ++ en w ++ "1" ++ ea a
  where
    en = unaryEncodeNumber
    ea = unaryEncodeAction

-- Run TM until HALT
run :: Machine -> [Machine]
run m
  | transition == Empty = [m]
  | otherwise = m : run (func transition m)
  where
    transition = choose m

-- Get transition to apply from machine
choose :: Machine -> Transition
choose (Machine q tape tr) = if q < (length tr) then getTransition (tr !! q) sym else Empty
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
transitionFromTuple :: (State, PrettySymbol, State, PrettySymbol, Action) -> [PrettySymbol] -> Transition
transitionFromTuple (qA, prettySA, qF, prettySW, act) alp = Transition {qA = qA, sA = sA, qF = qF, sW = sW, act = act, func = transitionMaker qF sW act}
  where
    sA = prettyToSymbol alp prettySA
    sW = prettyToSymbol alp prettySW

-- Create a transition directly with internal value
transitionFromTupleEncoded :: (State, Symbol, State, Symbol, Action) -> Transition
transitionFromTupleEncoded (qA, sA, qF, sW, act) = Transition {qA = qA, sA = sA, qF = qF, sW = sW, act = act, func = transitionMaker qF sW act}

-- Create transition function
transitionMaker :: State -> Symbol -> Action -> Machine -> Machine
transitionMaker q s act m@(Machine _ tape tr) = m {q = (q - 1), tape = move act . writeTape s $ tape}

-- alp = "1-=."
tWithRaw =
  [ [ -- tq1
      transitionFromTupleEncoded (1, 1, 1, 1, RIGHT),
      transitionFromTupleEncoded (1, 2, 1, 2, RIGHT),
      transitionFromTupleEncoded (1, 3, 2, 4, LEFT),
      transitionFromTupleEncoded (1, 4, 1, 4, RIGHT)
    ],
    [ -- tq2
      transitionFromTupleEncoded (2, 1, 3, 3, LEFT),
      transitionFromTupleEncoded (2, 2, 5, 4, LEFT)
    ],
    [ -- tq3
      transitionFromTupleEncoded (3, 1, 3, 1, LEFT),
      transitionFromTupleEncoded (3, 2, 4, 2, LEFT)
    ],
    [ -- tq4
      transitionFromTupleEncoded (4, 1, 1, 4, RIGHT),
      transitionFromTupleEncoded (4, 4, 4, 4, LEFT)
    ]
  ]
