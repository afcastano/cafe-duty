module App.Service (fetchTeam, currentDuty, nextDuty, allDuties, completeDuty) where

import App.RosterGeneration (generateRoster)
import App.Repository (Team(..), Person, findTeam, saveTeam)
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

completeDuty :: String -> IO ()
completeDuty teamName = do
          maybeTeam <- findTeam teamName
          case maybeTeam of
              Nothing -> return ()
              Just team ->  updateTeam team


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

updateTeam :: Team -> IO ()
updateTeam team = do
          let team' = team {rosterIndex = (rosterIndex team) + 1 }
          saveTeam team'



