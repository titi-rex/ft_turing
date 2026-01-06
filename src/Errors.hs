module Errors where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.List (elemIndex)
import qualified Data.Map as Map
import Types

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

  -- check that HALT is not a transition state
  if "HALT" `elem` transStates
    then Left $ "Error: 'HALT' cannot be used as a transition state"
    else Right ()

  -- check that blank symbol is not in the input
  -- TODO

  -- check that each action is either LEFT or RIGHT
  let actions = map Types.action allTransitions
      invalidActions = filter (\a -> a /= "LEFT" && a /= "RIGHT") actions
  if not (null invalidActions)
    then Left $ "Error: action(s) " ++ show invalidActions ++ " are invalid. It must be either 'LEFT' or 'RIGHT'"
    else Right ()

  -- check that each character of the alphabet is a string of length 1
  let invalidAlphabetChars = filter (\s -> length s /= 1) alphabetList
  if not (null invalidAlphabetChars)
    then Left $ "Error: alphabet character(s) " ++ show invalidAlphabetChars ++ " are invalid. Each character must be a string of length 1"
    else Right ()

  Right ()

-- ajout d'une fonction de verification d'erreur dans le parsing:
-- - verif que le blank est dans l'alphabet
-- - verif que les etats dans les transitions existent
-- - verif que les symboles dans les transitions existent dans l'alphabet
-- - verif que HALT n'est pas dans les transition
-- - blank ne doit pas etre dans l'input
-- - verif que l'action est soit left soit right
-- - Each character of the alphabet must be a string of length strictly equal to 1.
