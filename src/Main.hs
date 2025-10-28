module Main where

import Tape 
import Machine
import Parser

import Prelude
import Data.List
import Debug.Trace


-- type -> alias for type
-- data -> new type


debug label value =  trace (label ++ show value)

input = "111-11="

main :: IO ()
main = do
    let m = Machine{q=0, tape=fromString defaultBlank input, transitions=tQ, alphabet = "1-=."}

    print . alphabet $ m
    mapM_ print . concat . transitions $ m
    print "======================"
    mapM_  print . run $ m
