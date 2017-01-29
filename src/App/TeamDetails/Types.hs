{-# LANGUAGE DeriveGeneric #-}
module App.TeamDetails.Types (Person(..), TeamDetails(..), increaseRosterIndex, increaseTimesOnDuty, newTeam, newPerson, addPersonToTeam) where

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
instance Eq Person where
  Person n1 _ _ == Person n2 _ _ = n1 == n2

-- Person functions

increaseTimesOnDuty :: Person -> Person
increaseTimesOnDuty p = p {timesOnDuty = (timesOnDuty p) + 1}

newPerson :: String -> Person
newPerson personName = Person personName True 0


data TeamDetails = TeamDetails {
    teamName :: String
  , members :: [Person]
  , rosterIndex :: Int
} deriving ( Generic, Show )

instance ToJSON TeamDetails
instance FromJSON TeamDetails

-- Team functions
increaseRosterIndex :: TeamDetails -> TeamDetails
increaseRosterIndex t = t {rosterIndex = (rosterIndex t) + 1}

newTeam :: String -> TeamDetails
newTeam name = TeamDetails name [] 0

addPersonToTeam :: Person -> TeamDetails -> TeamDetails
addPersonToTeam person team = team {members = (members team)++[person]}