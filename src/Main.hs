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

runMachineFromJSON :: FilePath -> String -> IO ()
runMachineFromJSON jsonFile input = do
  runMachine <- createMachineFromJSON jsonFile input
  mapM_ print . concat . Machine.transitions $ runMachine
  putStrLn "************************************************"
  mapM_ print . run $ runMachine

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
              parser args input
              runMachineFromJSON args "111-11=" -- TODO: get input from args
            else putStrLn "Invalid arguments. Use -h or --help for more information."
