module App.Roster.Repository (findTeam, findTeamAndMap, saveTeam, saveMaybeTeam) where

import App.Roster.Types (Team(..))
import App.Helper.FileDB

findTeam :: String -> IO (Maybe Team)
findTeam name = findEntity "Team" name           

saveTeam :: Team -> IO ()
saveTeam team = saveEntity "Team" (teamName team) team

saveMaybeTeam :: Maybe Team -> IO ()
saveMaybeTeam Nothing     = return ()
saveMaybeTeam (Just team) = saveTeam team

findTeamAndMap :: (Team -> a) -> String -> IO (Maybe a)
findTeamAndMap mapper teamName = do
                            maybeTeam <- findTeam teamName
                            return $ mapper <$> maybeTeam

