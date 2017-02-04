{-# LANGUAGE DeriveGeneric #-}
module App.TeamDetails.Types (Person(..), TeamDetails(..), increaseTimesOnDuty, newTeam, newPerson, addPersonToTeam) where

import App.Helper.Lists (transformElem)

import Data.Aeson (FromJSON, ToJSON)
import GHC.Generics
import Data.List

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
newPerson :: String -> Person
newPerson personName = Person personName True 0


---- TeamDetails type
data TeamDetails = TeamDetails {
    teamName :: String
  , members :: [Person]
} deriving ( Generic, Show )

instance ToJSON TeamDetails
instance FromJSON TeamDetails

-- Functions
newTeam :: String -> TeamDetails
newTeam name = TeamDetails name []

-- TODO Validate person does not exist
addPersonToTeam :: Person -> TeamDetails -> TeamDetails
addPersonToTeam person team = team {members = (members team)++[person]}

-- TODO For the sake of studying, find another way to do this. Seems too imperative.
increaseTimesOnDuty :: TeamDetails -> (String, String) -> TeamDetails
increaseTimesOnDuty team (n1,n2) = let p1           = findPerson team n1
                                       p2           = findPerson team n2
                                       teamUpdated  = updatePersonOnTeam team p1
                                       newTeam      = updatePersonOnTeam teamUpdated p2
                                   in newTeam

--- Private
updatePersonOnTeam :: TeamDetails -> Maybe Person -> TeamDetails
updatePersonOnTeam team Nothing  = team
updatePersonOnTeam team (Just p) = team {members = transformElem increaseTimesOnDutyPerson p (members team)}

increaseTimesOnDutyPerson :: Person -> Person
increaseTimesOnDutyPerson p = p {timesOnDuty = (timesOnDuty p) + 1}

findPerson :: TeamDetails -> String -> Maybe Person
findPerson team pName = let people       = members team
                            sameName val = (name val) == pName
                        in  find sameName people





