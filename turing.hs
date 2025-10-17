import Prelude
import Data.List
import Debug.Trace
-- import Control.Lens

-- type -> alias for type
-- data -> new type

type State = Int
type Symbol = Char
type Tape = [Symbol]


data Action = LEFT | RIGHT deriving (Show)

data Transition = Transition {
    qA :: State,
    lR :: Symbol,
    qF :: State,
    lW :: Symbol,
    act :: Action
} deriving (Show)


data Machine = Machine {
    q :: State,
    tape :: Tape,
    idx :: Int
} deriving (Show)

-- Transitions
transitions = [
        [   -- TrQ1
            (1, 1, 2),
            (1, 2, 2),
            (2, 4, 1),
            (1, 4, 2)
        ],
        [   -- TrQ2
            (3, 3, 1),
            (5, 4, 1)
        ],
        [   -- TrQ3
            (3, 1, 1),
            (4, 2, 1)
        ],
        [   -- TrQ4
            (1, 4, 2),
            (-1, 0, 0),
            (-1, 0, 0),
            (4, 4, 1)
        ]
    ]


tQ = [
        [
        Transition{qA = 1, lR = '1', qF = 1, lW = '1', act = RIGHT},
        Transition{qA = 1, lR = '-', qF = 1, lW = '2', act = RIGHT},
        Transition{qA = 1, lR = '=', qF = 2, lW = '.', act = LEFT},
        Transition{qA = 1, lR = '.', qF = 1, lW = '.', act = RIGHT}
        ]
    ]



input = "11-11="
alphabet = "1-=."

debug label value =  trace (label ++ show value)

main :: IO ()
main = do
    let tm = Machine{q = 0, tape = input, idx = 0}
    print (tm)



-- writeTape tape head letter = set (element head) letter tape


--let (a,b) = splitAt 3 "foobar" in b ++ a 
-- newTape tape position newSymbol =
--    take (position) tape ++ newSymbol : drop (position+1) tape






