module Turing.Machine (Machine(..), Transition(..), transitionMaker, tQ, choose, next) where

import Data.List

import Turing.Tape
import Turing.State

import Debug.Trace

debug label value =  trace (label ++ show value)

-- Turing Machine representation 
data Machine = Machine {
    q :: State,
    tape :: Tape,
    transitions :: [[Transition]],
    alphabet :: [Symbol]
} 


-- Transition from one state to another where :
--  State to match
--  Symbol to match
--  transition function (TF)
data Transition = Transition {
    qA :: State,
    sA :: Symbol,
    func :: TransitionFunction
} 


-- Type to describe TF
type TransitionFunction = Machine -> Machine 

instance Show Transition where
    show (Transition qA sA _) = "Require: q=" ++ (show qA) ++ ", s=" ++ (show sA)


-- Create transition function
transitionMaker :: State -> Symbol -> Action -> Machine -> Machine
transitionMaker q s act (Machine _ tape tr alp) = (Machine q (move act . writeTape s $ tape) tr alp)


-- Create a transition from a 5-tuples (for code readability)
fromTuple :: (Int, Symbol, Int, Symbol, Action) -> Transition
fromTuple (qA, sA, qF, sW, act) = Transition{qA=qA, sA=sA, func = transitionMaker qF sW act}


-- Pick the next TF
choose :: Machine -> Transition
choose (Machine q tape tr alp) = ((tr !! q) !! sidx)
    where sidx = case (elemIndex value alp) of 
            Just x -> debug "idx found" x x
            Nothing -> debug "not found" (length alp) (length alp)
          value = readTape tape


next :: Machine -> Machine
next m@(Machine q tape tr alp) = f m
    where f = func . choose $ m


-- hard encoding of unary_sub machine transition
tQ = [
        [   -- tq1
            fromTuple (1, '1', 1, '1', RIGHT),
            fromTuple (1, '-', 1, '-', RIGHT),
            fromTuple (1, '=', 2, '.', LEFT),
            fromTuple (1, '.', 1, '.', RIGHT)
        ],
        [   -- tq2
            fromTuple (2, '1', 3, '=', LEFT),
            fromTuple (2, '-', 5, '.', LEFT)
        ],
        [   -- tq3
            fromTuple (3, '1', 3, '1', LEFT),
            fromTuple (3, '-', 4, '-', LEFT)
        ],
        [   -- tq4
            fromTuple (4, '1', 1, '.', RIGHT),
            fromTuple (-1, '.', 1, '.', RIGHT),
            fromTuple (-1, '.', 1, '.', RIGHT),
            fromTuple (4, '.', 4, '.', LEFT)
        ]
    ]

