module Print
  ( printMachineName,
    printAlphabet,
    printStates,
    printInitialState,
    printFinals,
    printTransitions,
  )
where

import qualified Data.Map as Map
import Types

printMachineName :: MachineJSON -> IO ()
printMachineName machine = do
  putStrLn "************************************************"
  putStrLn $ "*                  " ++ Types.name machine ++ "                    *"
  putStrLn "************************************************"

printAlphabet :: MachineJSON -> IO ()
printAlphabet machine = do
  putStrLn $ "Alphabet: [" ++ unwords (map (++ ",") (init (Types.alphabet machine))) ++ " " ++ last (Types.alphabet machine) ++ "]"

printStates :: MachineJSON -> IO ()
printStates machine = do
  putStrLn $ "States: [" ++ unwords (map (++ ",") (init (Types.states machine))) ++ " " ++ last (Types.states machine) ++ "]"

printInitialState :: MachineJSON -> IO ()
printInitialState machine = do
  putStrLn $ "Initial State: " ++ Types.initial machine

printFinals :: MachineJSON -> IO ()
printFinals machine = do
  putStrLn $ "Final States: [" ++ unwords (map (++ ",") (init (Types.finals machine))) ++ " " ++ last (Types.finals machine) ++ "]"

printTransitions :: MachineJSON -> IO ()
printTransitions machine = do
  putStrLn "Transitions:"
  mapM_ printStateTransitions (Map.toList (Types.transitions machine))
  putStrLn "************************************************"
  where
    printStateTransitions (state, trans) = do
      putStrLn $ "  " ++ state ++ ":"
      mapM_ (printTransition "    ") trans

printTransition :: String -> Transition -> IO ()
printTransition indent transition = do
  putStrLn $ indent ++ "(" ++ Types.read transition ++ ", " ++ Types.to_state transition ++ ") -> (" ++ Types.write transition ++ ", " ++ Types.action transition ++ ")"
