module App.Roster.Repository (findTeam, saveTeam) where

import App.Roster.Types (Team(..))
import App.Helper.FileDB

findTeam :: String -> IO (Maybe Team)
findTeam name = findEntity "Team" name           

saveTeam :: Team -> IO ()
saveTeam team = saveEntity "Team" (teamName team) team
