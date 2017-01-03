{-# LANGUAGE DeriveGeneric #-}
module App.Repository (Person, Team, allPeople, findTeam, myTeam) where

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics

import App.Helper.FileDB

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
} deriving ( Generic, Show )

instance ToJSON Team
instance FromJSON Team


bob :: Person
bob = Person { name = "Bob", active = True, timesOnDuty = 0 }

alice :: Person
alice = Person { name = "Alice", active = True, timesOnDuty = 0 }

eve :: Person
eve = Person { name = "Eve", active = True, timesOnDuty = 0 }

peter :: Person
peter = Person { name = "Peter", active = True, timesOnDuty = 0 }

p1 :: Person
p1 = Person { name = "P1", active = True, timesOnDuty = 0 }

p2 :: Person
p2 = Person { name = "P2", active = True, timesOnDuty = 0 }

allPeople :: [Person]
allPeople = [bob, alice, eve, peter, p1, p2]

myTeam :: Team
myTeam = Team { teamName = "Robusta", members = allPeople }

--- Data access function
findTeam :: String -> IO (Either String Team)
findTeam name = findEntity "Team" name                          