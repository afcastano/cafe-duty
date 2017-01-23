{-# LANGUAGE OverloadedStrings #-}
module App.Helper.FileDB (findEntity, saveEntity, listEntities) where

import App.Helper.Lists (dropLast)

import Data.Aeson (FromJSON, ToJSON, decode)
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString.Lazy.Char8
import Prelude as P
import System.Directory (listDirectory, createDirectoryIfMissing)
import Data.List as List

findEntity :: FromJSON a => String -> String -> IO (Maybe a)
findEntity entityKind name = do
                          stringVal <- readFileDB entityKind name
                          return $ decode stringVal -- decode from Json to the Record.

saveEntity :: ToJSON a => String -> String -> a -> IO () -- Consider returning the id
saveEntity entityKind entityId entity = do
                          let encodedEntity = encodePretty entity -- Encode the entity to JSON
                          writeFileDB entityKind entityId encodedEntity

listEntities :: String -> IO [String]
listEntities entityKind = do
                          fileNames <- listDirectory $ buildDbDir entityKind
                          return $ List.map (dropLast 5) fileNames


-- Not exposed functions
-- File system manipulation
writeFileDB :: String -> String -> ByteString -> IO ()
writeFileDB path name content = do
    createDbDir path
    let contentStr = unpack content -- From ByteString to String
    -- `seq` forces the evaluation of val before writing.
    -- http://stackoverflow.com/questions/2527271/in-haskell-i-want-to-read-a-file-and-then-write-to-it-do-i-need-strictness-ann
    List.length contentStr `seq` P.writeFile (buildDbFilePath path name) contentStr

readFileDB :: String -> String -> IO ByteString
readFileDB path name = do
    content <- P.readFile $ buildDbFilePath path name
    return $ pack content -- Transform to byteString and return



createDbDir :: String -> IO ()
createDbDir path = createDirectoryIfMissing True ("db/"++path)

buildDbDir :: String -> String
buildDbDir relativePath = "db/"++relativePath

buildDbFilePath :: String -> String -> String
buildDbFilePath path name = (buildDbDir path)++"/"++name++".json"