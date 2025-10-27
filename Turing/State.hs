module Turing.State (State,) where

import Turing.Tape (Symbol, Action(..), Writer)

-- Machine state (STATE) represented by int value
type State = Int




-- Transition function where :
--  State to match
--  Symbol to match
--  new State
--  Symbol to write
--  Action
data Transition = Transition {
    qA :: State,
    sA :: Symbol,
    qF :: State,
    sW :: Symbol,
    act :: Action
} deriving (Show)


-- Create a transition from a 5-tuples (for code readability)
fromTuple :: (Int, Symbol, Int, Symbol, Action) -> Transition
fromTuple (qA, sA, qF, sW, act) = Transition{qA=qA, sA=sA, qF=qF, sW=sW, act=act}





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
            fromTuple (4, '.', 4, '.', LEFT)
        ]
    ]

