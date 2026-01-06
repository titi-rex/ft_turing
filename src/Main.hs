module Main where

import Data.List
import Data.Maybe
import Machine
import Parser
import Tape
import Prelude

input = "111-11="

encoder :: IO ()
encoder = do
  print . encode $ Machine {q = 0, tape = fromString defaultBlank input, transitions = tQ, alphabet = "1-=."}

main :: IO ()
main = do
  let m = Machine {q = 0, tape = fromString defaultBlank input, transitions = tQ, alphabet = "1-=."}

  print . alphabet $ m
  mapM_ print . concat . transitions $ m
  print "======================"
  mapM_ print . run $ m
