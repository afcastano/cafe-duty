{-# LANGUAGE DeriveDataTypeable #-}
module App.Roster.Repository (findTeam, getTeam, findTeamAndMap, saveTeam, saveMaybeTeam, getTeamsName, saveNewTeam) where

import App.Roster.Types (Team(..))
import App.Helper.FileDB(listEntities, findEntity, saveEntity)

import Control.Exception
import Data.Typeable

findTeam :: String -> IO (Maybe Team)
findTeam name = findEntity "Team" name

getTeam :: String -> IO (Either String Team)
getTeam name = do 
            maybeTeam <- findTeam name
            return $ toEither "Team does not exist" maybeTeam           

saveTeam :: Team -> IO ()
saveTeam team = saveEntity "Team" (teamName team) team

saveNewTeam :: Team -> IO (Either String ())
saveNewTeam team = do
            maybeTeam <- findTeam (teamName team)
            case maybeTeam of
                Nothing -> tryStr (saveTeam team)
                Just _ -> return (Left "Team already exists!")

saveMaybeTeam :: Maybe Team -> IO ()
saveMaybeTeam Nothing     = return ()
saveMaybeTeam (Just team) = saveTeam team

findTeamAndMap :: (Team -> a) -> String -> IO (Maybe a)
findTeamAndMap mapper teamName = do
                            maybeTeam <- findTeam teamName
                            return $ mapper <$> maybeTeam

getTeamsName :: IO [String]
getTeamsName =  listEntities "Team"    


-- Repo Exception types
data RepoException = RepoException String
    deriving (Typeable)

instance Exception RepoException

instance Show RepoException where
    show (RepoException e) = show e

-- Helper
toEither:: String -> Maybe a -> Either String a
toEither msg Nothing = Left msg
toEither _ (Just val) = Right val

tryStr :: IO a -> IO (Either String a)
tryStr io = do
          result <- try io
          case result of
            Left e -> return $ Left $ show (e :: SomeException)
            Right a -> return $ Right a

