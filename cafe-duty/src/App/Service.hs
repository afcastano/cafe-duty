module App.Service (fetchTeam, currentDuty, nextDuty, getAllDuties, completeDuty) where

import App.RosterGeneration (generateRoster)
import App.Repository (Team(..), Person, findTeam, saveTeam)
import App.Helper.Lists (rotate)

fetchTeam :: String -> IO (Maybe Team) -- Return a result object better
fetchTeam name = findTeam name

currentDuty :: String -> IO [Person]
currentDuty teamName = getDutyForIndex teamName 0

nextDuty :: String -> IO [Person]
nextDuty teamName = getDutyForIndex teamName 1
          
getAllDuties :: String -> IO [[Person]]
getAllDuties teamName =  generateCurrentRoster <$> (findTeam teamName)

completeDuty :: String -> IO ()
completeDuty teamName = completeDutyOnTeam =<< (findTeam teamName)

---------- Helper functions----
generateCurrentRoster :: Maybe Team -> [[Person]]
generateCurrentRoster Nothing     = [] 
generateCurrentRoster (Just team) = 
                          let roster  = generateRoster $ members team
                          in rotate (rosterIndex team) roster

getDutyForIndex :: String -> Int -> IO [Person]
getDutyForIndex teamName idx = (getIndexSafe idx) <$> (getAllDuties teamName)

getIndexSafe :: Int -> [[a]] -> [a]
getIndexSafe _ []        = []
getIndexSafe index list  = (cycle list) !! index

completeDutyOnTeam :: Maybe Team -> IO ()
completeDutyOnTeam Nothing      = return ()
completeDutyOnTeam (Just team)  = saveTeam $ team {rosterIndex = (rosterIndex team) + 1}


