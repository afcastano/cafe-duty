{-# LANGUAGE DeriveDataTypeable #-}
module App.TeamDetails.Repository (findTeam, getTeam, findTeamAndMap, saveTeam, saveMaybeTeam, getTeamNames, saveNewTeam) where

import App.TeamDetails.Types (TeamDetails(..))
import App.Helper.FileDB(listEntities, findEntity, saveEntity)

import Control.Exception
import Data.Typeable

findTeam :: String -> IO (Maybe TeamDetails)
findTeam name = findEntity "TeamDetails" name

getTeam :: String -> IO (Either String TeamDetails)
getTeam name = do 
            maybeTeam <- findTeam name
            return $ toEither "Team does not exist" maybeTeam           

saveTeam :: TeamDetails -> IO ()
saveTeam team = saveEntity "TeamDetails" (teamName team) team

saveNewTeam :: TeamDetails -> IO (Either String TeamDetails)
saveNewTeam team = do
            maybeTeam <- findTeam (teamName team)
            case maybeTeam of
                Nothing -> tryStr (saveTeam team >> return team)
                Just _ -> return (Left "Team already exists!")

saveMaybeTeam :: Maybe TeamDetails -> IO ()
saveMaybeTeam Nothing     = return ()
saveMaybeTeam (Just team) = saveTeam team

findTeamAndMap :: (TeamDetails -> a) -> String -> IO (Maybe a)
findTeamAndMap mapper teamName = do
                            maybeTeam <- findTeam teamName
                            return $ mapper <$> maybeTeam

getTeamNames :: IO [String]
getTeamNames =  listEntities "TeamDetails"

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

