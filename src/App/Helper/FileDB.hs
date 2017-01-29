{-# LANGUAGE OverloadedStrings #-}
module App.Helper.FileDB (findEntity, saveEntity, listEntities) where

import App.Helper.Lists (dropLast)

import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8
import Prelude as P
import System.Directory (listDirectory, createDirectoryIfMissing, doesDirectoryExist)
import Data.List as List
import Control.Exception (try)

findEntity :: FromJSON a => String -> String -> IO (Maybe a)
findEntity entityKind name = do
                          maybeStringVal <- readFileDB entityKind name
                          return $ decode =<< maybeStringVal -- decode from Json to the Record.

saveEntity :: ToJSON a => String -> String -> a -> IO () -- Consider returning the id
saveEntity entityKind entityId entity = do
                          let encodedEntity = encodePretty entity -- Encode the entity to JSON
                          writeFileDB entityKind entityId encodedEntity

listEntities :: String -> IO [String]
listEntities entityKind = do
                          fileNames <- getFileNamesSafe $ buildDbDir entityKind
                          return $ List.map (dropLast 5) fileNames


-- PRIVATE
getFileNamesSafe :: String -> IO [String]
getFileNamesSafe path = do
                    exists <- doesDirectoryExist path
                    if exists then listDirectory path else return []

-- File system manipulation
writeFileDB :: String -> String -> ByteString -> IO ()
writeFileDB path name content = do
    createDbDir path
    let contentStr = unpack content -- From ByteString to String
    -- `seq` forces the evaluation of val before writing.
    -- http://stackoverflow.com/questions/2527271/in-haskell-i-want-to-read-a-file-and-then-write-to-it-do-i-need-strictness-ann
    List.length contentStr `seq` P.writeFile (buildDbFilePath path name) contentStr

readFileDB :: String -> String -> IO (Maybe ByteString)
readFileDB path name = do
        eitherContent <- try (P.readFile $ buildDbFilePath path name)
        case (eitherContent :: Either IOError String) of 
          Left _    -> return $ Nothing
          Right val -> return $ Just $ pack val -- Transform to byteString and return
    



createDbDir :: String -> IO ()
createDbDir path = createDirectoryIfMissing True ("db/"++path)

buildDbDir :: String -> String
buildDbDir relativePath = "db/"++relativePath

buildDbFilePath :: String -> String -> String
buildDbFilePath path name = (buildDbDir path)++"/"++name++".json"