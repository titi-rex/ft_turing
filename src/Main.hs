module Main where

import Tape 
import Machine
import Parser

import Prelude
import Data.List


input = "111-11="

main :: IO ()
main = do
    let m = Machine{q=0, tape=fromString defaultBlank input, transitions=tQ, alphabet = "1-=."}

    print . alphabet $ m
    mapM_ print . concat . transitions $ m
    print "======================"
    mapM_  print . run $ m
