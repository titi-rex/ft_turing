{-# LANGUAGE DeriveGeneric #-}

module Parser where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics

data MachineJSON = MachineJSON
  { name :: String,
    alphabet :: [String],
    blank :: String,
    states :: [String],
    initial :: String,
    finals :: [String]
  }
  deriving (Show, Generic)

instance FromJSON MachineJSON

printMachineName :: MachineJSON -> IO ()
printMachineName machine = do
  putStrLn "************************************************"
  putStrLn $ "*                  " ++ name machine ++ "                    *"
  putStrLn "************************************************"

printAlphabet :: MachineJSON -> IO ()
printAlphabet machine = do
  putStrLn $ "Alphabet: [" ++ unwords (map (++ ",") (init (alphabet machine))) ++ " " ++ last (alphabet machine) ++ "]"

printStates :: MachineJSON -> IO ()
printStates machine = do
  putStrLn $ "States: [" ++ unwords (map (++ ",") (init (states machine))) ++ " " ++ last (states machine) ++ "]"

printInitialState :: MachineJSON -> IO ()
printInitialState machine = do
  putStrLn $ "Initial State: " ++ initial machine

printFinals :: MachineJSON -> IO ()
printFinals machine = do
  putStrLn $ "Final States: [" ++ unwords (map (++ ",") (init (finals machine))) ++ " " ++ last (finals machine) ++ " " ++ "]"

getMachineJson :: FilePath -> IO ()
getMachineJson filePath = do
  contents <- readFile filePath
  let decoded = decode (BL.pack contents) :: Maybe MachineJSON
  case decoded of
    Just machine -> do
      printMachineName machine
      printAlphabet machine
      printStates machine
      printInitialState machine
      printFinals machine
    Nothing -> putStrLn $ "Failed to parse JSON. Missing field " -- Todo: precise which field is missing

parser :: FilePath -> IO ()
parser filePath = do
  contents <- readFile filePath
  putStrLn "Fichier json : Turing machine implementation is currently being worked on. Please be patient."

  getMachineJson filePath
