module Main where

import Data.List
import Data.Maybe
import Encoding
import Machine
import Parser
import Tape

input = "111-11="

encodedInput = prettyToSymbolString alpha1 input

m1 = Machine {q = 0, tape = fromEncodedString alpha1 encodedInput, transitions = tWithEncoding}

alpha1 = "1-=."

pcode :: IO ()
pcode = putStrLn . encode $ m1

main :: IO ()
main = do
  putStrLn . alphabet . tape $ m1

  mapM_ print . concat . transitions $ m1
  putStrLn . encode $ m1
  putStrLn "#======================#"
  mapM_ print . run $ m1

-- hardcoding of unary_sub machine transitions
tWithEncoding =
  [ [ -- tq1
      transitionFromTuple (1, '1', 1, '1', RIGHT) alpha1,
      transitionFromTuple (1, '-', 1, '-', RIGHT) alpha1,
      transitionFromTuple (1, '=', 2, '.', LEFT) alpha1,
      transitionFromTuple (1, '.', 1, '.', RIGHT) alpha1
    ],
    [ -- tq2
      transitionFromTuple (2, '1', 3, '=', LEFT) alpha1,
      transitionFromTuple (2, '-', 5, '.', LEFT) alpha1
    ],
    [ -- tq3
      transitionFromTuple (3, '1', 3, '1', LEFT) alpha1,
      transitionFromTuple (3, '-', 4, '-', LEFT) alpha1
    ],
    [ -- tq4
      transitionFromTuple (4, '1', 1, '.', RIGHT) alpha1,
      transitionFromTuple (4, '.', 4, '.', LEFT) alpha1
    ]
  ]
