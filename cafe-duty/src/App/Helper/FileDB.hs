{-# LANGUAGE OverloadedStrings #-}
module App.Helper.FileDB (findEntity, saveEntity) where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import Data.ByteString.Lazy.Char8
import Prelude as P
import System.Directory
import Data.List as List

findEntity :: FromJSON a => String -> String -> IO (Maybe a)
findEntity entityKind name = do
                          stringVal <- readFileDB entityKind name
                          return $ decode stringVal -- decode from Json to the Record.

saveEntity :: ToJSON a => String -> String -> a -> IO () -- Consider returning the id
saveEntity entityKind entityId entity = do
                          let encodedEntity = encode entity -- Encode the entity to JSON
                          writeFileDB entityKind entityId encodedEntity


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

buildDbFilePath :: String -> String -> String
buildDbFilePath path name = "db/"++path++"/"++name++".json"