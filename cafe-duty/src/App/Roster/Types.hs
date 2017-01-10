{-# LANGUAGE DeriveGeneric #-}
module App.Roster.Types (Person(..), Team(..), increaseRosterIndex, increaseTimesOnDuty) where

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


data Team = Team {
    teamName :: String
  , members :: [Person]
  , rosterIndex :: Int
} deriving ( Generic, Show )

instance ToJSON Team
instance FromJSON Team

-- Team functions
increaseRosterIndex :: Team -> Team
increaseRosterIndex t = t {rosterIndex = (rosterIndex t) + 1}