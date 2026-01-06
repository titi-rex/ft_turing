{-# LANGUAGE DeriveGeneric #-}

module Parser where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (elemIndex)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)
import Machine
import Print
import Tape
import Types

parseMachineJSON :: FilePath -> IO (Either String MachineJSON)
parseMachineJSON filePath = do
  contents <- readFile filePath
  return $ eitherDecode (BL.pack contents)

printMachineJson :: FilePath -> IO ()
printMachineJson filePath = do
  decoded <- parseMachineJSON filePath
  case decoded of
    Right machine -> do
      case checkParsingErrors machine of
        Left errMsg -> error errMsg
        Right () -> return ()
      printMachineName machine
      printAlphabet machine
      printStates machine
      printInitialState machine
      printFinals machine
      printTransitions machine
    Left err -> putStrLn $ "Invalid JSON format: " ++ err

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

-- check all error cases in the JSON file
checkParsingErrors :: MachineJSON -> Either String ()
checkParsingErrors machineJson = do
  -- check if the blanck symbol is in the alphabet
  let blankSymbol = Types.blank machineJson
      alphabetList = Types.alphabet machineJson
  if blankSymbol `notElem` alphabetList
    then Left $ "Error: blank character '" ++ blankSymbol ++ "' must be part of the alphabet"
    else Right ()

  -- check if the initial state exists
  let initialState = Types.initial machineJson
      statesList = Types.states machineJson
  if initialState `notElem` statesList
    then Left $ "Error: initial state '" ++ initialState ++ "' is not in the states list"
    else Right ()

  -- check if final states exist
  let finalsList = Types.finals machineJson
      invalidFinals = filter (`notElem` statesList) finalsList
  if not (null invalidFinals)
    then Left $ "Error: final state(s) " ++ show invalidFinals ++ " are not in the states list"
    else Right ()

  -- check if states in transitions exist
  let transMap = Types.transitions machineJson
      transStates = Map.keys transMap
      invalidTransStates = filter (`notElem` statesList) transStates
  if not (null invalidTransStates)
    then Left $ "Error: transition state(s) " ++ show invalidTransStates ++ " are not in the states list"
    else Right ()

  -- check if to_state in each transition exists
  let allTransitions = concat $ Map.elems transMap
      toStates = map Types.to_state allTransitions
      invalidToStates = filter (`notElem` statesList) toStates
  if not (null invalidToStates)
    then Left $ "Error: target state(s) " ++ show invalidToStates ++ " in transitions are not in the states list"
    else Right ()

  -- check if symbols in transitions exist in alphabet
  let readSymbols = map Types.read allTransitions
      writeSymbols = map Types.write allTransitions
      allSymbols = readSymbols ++ writeSymbols
      invalidSymbols = filter (`notElem` alphabetList) allSymbols
  if not (null invalidSymbols)
    then Left $ "Error: symbol(s) " ++ show invalidSymbols ++ " in transitions are not in the alphabet"
    else Right ()

  Right ()

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
            Machine.alphabet = alphabetSymbols
          }
    Left _ -> error "Failed to parse JSON"

parser :: FilePath -> String -> IO ()
parser filePath input = do
  printMachineJson filePath

-- ajout d'une fonction de verification d'erreur dans le parsing:
-- - verif que le blank est dans l'alphabet
-- - verif que les etats dans les transitions existent
-- - verif que les symboles dans les transitions existent dans l'alphabet
