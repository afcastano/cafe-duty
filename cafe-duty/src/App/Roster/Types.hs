{-# LANGUAGE DeriveGeneric #-}
module App.Roster.Types (Person(..), Team(..)) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

-- Types and instances
data Person = Person {
    name :: String
  , active :: Bool
  , timesOnDuty :: Int
} deriving ( Generic, Show )

instance ToJSON Person
instance FromJSON Person

data Team = Team {
    teamName :: String
  , members :: [Person]
  , rosterIndex :: Int
} deriving ( Generic, Show )

instance ToJSON Team
instance FromJSON Team