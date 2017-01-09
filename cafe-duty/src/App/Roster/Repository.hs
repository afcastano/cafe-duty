module App.Roster.Repository (findTeam, saveTeam, saveMaybe) where

import App.Roster.Types (Team(..))
import App.Helper.FileDB

findTeam :: String -> IO (Maybe Team)
findTeam name = findEntity "Team" name           

saveTeam :: Team -> IO ()
saveTeam team = saveEntity "Team" (teamName team) team

saveMaybe :: Maybe Team -> IO ()
saveMaybe Nothing = return ()
saveMaybe (Just team) = saveTeam team

