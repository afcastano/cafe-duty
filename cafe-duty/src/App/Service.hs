module App.Service (fetchTeam, currentDuty, nextDuty, getAllDuties, completeDuty) where

import App.RosterGeneration (combineElements)
import App.Repository (Team(..), Person, findTeam, saveTeam)
import App.Helper.Lists (rotate)

fetchTeam :: String -> IO (Maybe Team) -- Return a result object better
fetchTeam name = findTeam name

currentDuty :: String -> IO [Person]
currentDuty teamName = getFst <$> getAllDuties teamName

nextDuty :: String -> IO [Person]
nextDuty teamName = getSnd <$> getAllDuties teamName
          
getAllDuties :: String -> IO [[Person]]
getAllDuties teamName =  generateCurrentRoster <$> findTeam teamName

completeDuty :: String -> IO ()
completeDuty teamName = completeDutyOnTeam =<< findTeam teamName

--- Helper IO functions
completeDutyOnTeam :: Maybe Team -> IO ()
completeDutyOnTeam Nothing      = return ()
completeDutyOnTeam (Just team)  = saveTeam $ team {rosterIndex = (rosterIndex team) + 1}


---------- Helper functions (Pure)----
generateCurrentRoster :: Maybe Team -> [[Person]]
generateCurrentRoster Nothing     = [] 
generateCurrentRoster (Just team) = 
                          let roster  = combineElements $ members team
                          in rotate (rosterIndex team) roster

getIndexSafe :: Int -> [[a]] -> [a]
getIndexSafe _ []        = []
getIndexSafe index list  = (cycle list) !! index

getFst :: [[a]] -> [a]
getFst = getIndexSafe 0

getSnd :: [[a]] -> [a]
getSnd = getIndexSafe 1

