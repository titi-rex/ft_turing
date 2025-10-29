module Main where

import Data.List
import Data.Maybe
import Machine
import Parser
import Tape
import Prelude

input = "111-11="

{--
other index can be arbitrary

state:
q1: initial ->  0
(q2: accept  ->  00
q3: reject  ->  000)
q4: halting ->  0000  -> no accept/reject ->  q2  -> 00

symbol:
s1: 0     ->  0
s2: 1     ->  00
s3: blank ->  000

direction:
left  ->  0
right ->  00
hold  ->  000

unary_encode :: [[Transition]] -> String
unary_encode [] = "1"
unary_encode (x : xs) = foldl (\acc x -> if x == y then True else acc) 1

--}

encode_transition :: Transition -> String
encode_transition (Transition q r f w a _) = eq q ++ "1" ++ es r ++ "1" ++ eq f ++ "1" ++ es w ++ "1" ++ ea a
  where
    eq = encode_number
    es = encode_number
    ea = encode_action

encode_number :: Int -> String
encode_number 0 = ""
encode_number q = "0" ++ encode_number (q - 1)

encode_symbol :: [Symbol] -> Symbol -> String
encode_symbol alp s = encode_number (idx + 1)
  where
    idx = fromMaybe (2) $ elemIndex s alp

encode_action :: Action -> String
encode_action LEFT = "0"
encode_action RIGHT = "00"

alpha1 = "1-=."

test :: IO ()
test = do
  let encoded = toEncodedString alpha1 input
  let m = Machine {q = 0, tape = fromEncodedString alpha1 encoded, transitions = tQ}
  print . choose $ m
  print . encode_transition . choose $ m

main :: IO ()
main = do
  let encoded = toEncodedString alpha1 input
  let m = Machine {q = 0, tape = fromEncodedString alpha1 encoded, transitions = tQ}

  print . alphabet . tape $ m
  mapM_ print . concat . transitions $ m
  print "======================"
  mapM_ print . run $ m
