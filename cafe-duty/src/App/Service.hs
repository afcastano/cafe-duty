module App.Service (fetchTeam, currentDuty, nextDuty, allDuties) where

import App.RosterGeneration
import App.Repository
import App.Helper.Lists (rotate)
import Data.Maybe

fetchTeam :: String -> IO (Maybe Team) -- Return a result object better
fetchTeam name = findTeam name

currentDuty :: String -> IO [Person]
currentDuty teamName = do
          maybeTeam <- findTeam teamName
          return $ ((!!0).generateRosterFrom) `bindToMaybe` maybeTeam -- make !!0 safer

nextDuty :: String -> IO [Person]
nextDuty teamName = do
          maybeTeam <- findTeam teamName
          return $ ((!!1).generateRosterFrom) `bindToMaybe` maybeTeam -- make !!1 safer
          
allDuties :: String -> IO [[Person]]
allDuties teamName = do
          maybeTeam <- findTeam teamName
          return $ generateRosterFrom `bindToMaybe` maybeTeam


---------- Helper functions----
generateRosterFrom :: Team -> [[Person]]
generateRosterFrom team = 
                  let roster  = generateRoster $ members team
                  in rotate (rosterIndex team) roster

bindToMaybe :: (a -> [b]) -> Maybe a -> [b]
bindToMaybe f Nothing  = []
bindToMaybe f (Just a) = f a 
