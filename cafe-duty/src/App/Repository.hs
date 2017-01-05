{-# LANGUAGE DeriveGeneric #-}
module App.Repository (Person, Team(..), findTeam) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

import App.Helper.FileDB

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

-- Data acces methods
findTeam :: String -> IO (Maybe Team)
findTeam name = findEntity "Team" name           

saveTeam :: Team -> IO ()
saveTeam team = saveEntity "Team" (teamName team) team
