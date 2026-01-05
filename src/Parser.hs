{-# LANGUAGE DeriveGeneric #-}

module Parser where

import Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BL
import GHC.Generics

data MachineJSON = MachineJSON
  { name :: String
  }
  deriving (Show, Generic)

instance FromJSON MachineJSON

printMachineName :: MachineJSON -> IO ()
printMachineName machine = do
  putStrLn "************************************************"
  putStrLn $ "*                  " ++ name machine ++ "                    *"
  putStrLn "************************************************"

parser :: FilePath -> IO ()
parser filePath = do
  contents <- readFile filePath
  putStrLn "Fichier json : Turing machine implementation is currently being worked on. Please be patient."

  -- recupere le contenu du fichier et le parse
  let decoded = decode (BL.pack contents) :: Maybe MachineJSON
  case decoded of
    Just machine -> printMachineName machine
    Nothing -> putStrLn $ "Failed to parse JSON. Missing field " -- TODO : ajouter le field manquant
