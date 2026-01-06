{-# LANGUAGE DeriveGeneric #-}

module Types where

import Data.Aeson
import qualified Data.Map as Map
import GHC.Generics

data MachineJSON = MachineJSON
  { name :: String,
    alphabet :: [String],
    blank :: String,
    states :: [String],
    initial :: String,
    finals :: [String],
    transitions :: Map.Map String [Transition]
  }
  deriving (Show, Generic)

data Transition = Transition
  { read :: String,
    to_state :: String,
    write :: String,
    action :: String
  }
  deriving (Show, Generic)

instance FromJSON MachineJSON

instance FromJSON Transition
