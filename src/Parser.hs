{-# LANGUAGE DeriveGeneric #-}

module Parser where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (elemIndex)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Errors
import Machine
import Print
import Tape
import Types

-- convert JSON action string to Action type
convertAction :: String -> Action
convertAction "LEFT" = LEFT
convertAction "RIGHT" = RIGHT
convertAction _ = RIGHT -- default

-- convert a single MachineJSON Transition to Machine.Transition
convertTransition :: [String] -> Int -> Types.Transition -> Machine.Transition
convertTransition statesList fromStateIdx jsonTrans =
  let toStateIdx = fromMaybe 0 $ elemIndex (Types.to_state jsonTrans) statesList
      readSym = head (Types.read jsonTrans)
      writeSym = head (Types.write jsonTrans)
      action = convertAction (Types.action jsonTrans)
   in Machine.fromTuple (fromStateIdx + 1, readSym, toStateIdx + 1, writeSym, action)

-- convert all transitions to Machine format
convertTransitions :: MachineJSON -> [[Machine.Transition]]
convertTransitions machineJson =
  let statesList = Types.states machineJson
      transMap = Types.transitions machineJson
   in map
        ( \(idx, state) ->
            let jsonTransitions = fromMaybe [] (Map.lookup state transMap)
             in map (convertTransition statesList idx) jsonTransitions
        )
        (zip [0 ..] statesList)

createMachineFromJSON :: FilePath -> String -> IO Machine
createMachineFromJSON filePath input = do
  decoded <- parseMachineJSON filePath
  case decoded of
    Right machineJson -> do
      -- convert alphabet to the right format with blank at the end
      let blankSymbol = Types.blank machineJson
          alphabetList = Types.alphabet machineJson
          alphabetWithoutBlank = filter (/= blankSymbol) alphabetList
          reorderedAlphabet = alphabetWithoutBlank ++ [blankSymbol]
          alphabetSymbols = concat reorderedAlphabet
          blankChar = head blankSymbol

          initialState = Types.initial machineJson
          statesList = Types.states machineJson

          initialStateIndex = fromMaybe 0 $ elemIndex initialState statesList
          tapeFromInput = fromString blankChar input
          transitionTable = convertTransitions machineJson
      return
        Machine
          { q = initialStateIndex,
            tape = tapeFromInput,
            Machine.transitions = transitionTable,
            Machine.alphabet = alphabetSymbols,
            prettyStates = statesList
          }
    Left _ -> error "Failed to parse JSON"

parseMachineJSON :: FilePath -> IO (Either String MachineJSON)
parseMachineJSON filePath = do
  contents <- readFile filePath
  return $ eitherDecode (BL.pack contents)

parser :: FilePath -> String -> IO ()
parser filePath input = do
  decoded <- parseMachineJSON filePath
  case decoded of
    Right machine -> do
      case checkParsingErrors machine input of
        Left errMsg -> error errMsg
        Right () -> return ()
      printMachineName machine
      printAlphabet machine
      printStates machine
      printInitialState machine
      printFinals machine
    Left err -> putStrLn $ "Invalid JSON format: " ++ err
