module App.Service (fetchTeam, currentDuty, nextDuty, allDuties) where

import App.RosterGeneration (generateRoster)
import App.Repository (Team(..), Person, findTeam)
import App.Helper.Lists (rotate)

fetchTeam :: String -> IO (Maybe Team) -- Return a result object better
fetchTeam name = findTeam name

currentDuty :: String -> IO [Person]
currentDuty teamName = dutyOnIndex teamName 0

nextDuty :: String -> IO [Person]
nextDuty teamName = dutyOnIndex teamName 1
          
allDuties :: String -> IO [[Person]]
allDuties teamName = do
          maybeTeam <- findTeam teamName
          return $ currentRosterFrom maybeTeam


---------- Helper functions----
currentRosterFrom :: Maybe Team -> [[Person]]
currentRosterFrom Nothing = [] 
currentRosterFrom (Just team) = 
                                let roster  = generateRoster $ members team
                                in rotate (rosterIndex team) roster

dutyOnIndex :: String -> Int -> IO [Person]
dutyOnIndex teamName index = do
          roster <- allDuties teamName
          return $ roster `getSafe` index

getSafe :: [[a]] -> Int -> [a]
getSafe [] _        = []
getSafe list index  = (cycle list) !! index



