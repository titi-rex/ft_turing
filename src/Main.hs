module Main where

import Data.List
import Data.Maybe
import Machine
import Parser
import System.Environment (getArgs)
import System.Exit (exitSuccess)
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

encode_transition :: [Symbol] -> Transition -> String
encode_transition alp (Transition q r f w a _) = eq q ++ "1" ++ es alp r ++ "1" ++ eq f ++ "1" ++ es alp w ++ "1" ++ ea a
  where
    eq = encode_number
    es = encode_symbol
    ea = encode_action

encode_number :: State -> String
encode_number 0 = ""
encode_number q = "0" ++ encode_number (q - 1)

encode_symbol :: [Symbol] -> Symbol -> String
encode_symbol alp s = encode_number (idx + 1)
  where
    idx = fromMaybe (2) $ elemIndex s alp

encode_action :: Action -> String
encode_action LEFT = "0"
encode_action RIGHT = "00"

test :: IO ()
test = do
  let m = Machine {q = 0, tape = fromString defaultBlank input, transitions = tQ, Machine.alphabet = "1-=."}
  print . choose $ m
  print . encode_transition "1-=." . choose $ m

-- main :: IO ()
-- main = do
--   let m = Machine {q = 0, tape = fromString defaultBlank input, transitions = tQ, alphabet = "1-=."}

--   print . alphabet $ m
--   mapM_ print . concat . transitions $ m
--   print "======================"
--   mapM_ print . run $ m

showHelp :: IO ()
showHelp = do
  putStrLn "usage: ft_turing [-h] jsonfile input"
  putStrLn ""
  putStrLn "positional arguments:"
  putStrLn "  jsonfile             json description of the machine"
  putStrLn "  input                input of the machine"
  putStrLn ""
  putStrLn "optional arguments:"
  putStrLn "  -h, --help          show this help message and exit"

main :: IO ()
main = do
  argsList <- getArgs
  if null argsList
    then putStrLn "No arguments provided. Use -h or --help for more information."
    else do
      let args = head argsList
      if args == "-h" || args == "--help"
        then showHelp >> exitSuccess
        else
          if ".json" `isSuffixOf` args
            then do
              -- TODO: implement json parsing
              parser args
            else putStrLn "Invalid arguments. Use -h or --help for more information."
