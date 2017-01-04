{-# LANGUAGE OverloadedStrings #-}
module App.Helper.FileDB (findEntity, saveEntity) where

import Data.Aeson (FromJSON, ToJSON, eitherDecode, encode)
import Data.ByteString.Lazy.Char8
import Prelude as P
import System.Directory
import Data.List as List

findEntity :: FromJSON a => String -> String -> IO (Either String a)
findEntity entityKind name = do
                          stringVal <- readByteStringDb entityKind name
                          return $ eitherDecode stringVal

saveEntity :: ToJSON a => String -> String -> a -> IO () -- Consider returning the id
saveEntity entityKind entityId entity = do
                          let encodedEntity = encode entity
                          writeByteStringDb entityKind entityId encodedEntity

-- File system manipulation
writeByteStringDb :: String -> String -> ByteString -> IO ()
writeByteStringDb path name content = do
    createDbDir path
    let contentStr = unpack content
    -- `seq` forces the evaluation of val before writing.
    -- http://stackoverflow.com/questions/2527271/in-haskell-i-want-to-read-a-file-and-then-write-to-it-do-i-need-strictness-ann
    List.length contentStr `seq` P.writeFile (buildDbFilePath path name) contentStr

readByteStringDb :: String -> String -> IO ByteString
readByteStringDb path name = do
    content <- P.readFile $ buildDbFilePath path name
    return $ pack content

createDbDir :: String -> IO ()
createDbDir path = createDirectoryIfMissing True ("db/"++path)

buildDbFilePath :: String -> String -> String
buildDbFilePath path name = "db/"++path++"/"++name++".json"