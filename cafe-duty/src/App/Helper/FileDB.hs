{-# LANGUAGE OverloadedStrings #-}
module App.Helper.FileDB (findEntity) where

import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy.Char8
import Prelude as P

findEntity :: FromJSON a => String -> String -> IO (Either String a)
findEntity path name = do
                          stringVal <- readStringDb path name
                          return (eitherDecode $ pack stringVal)

readStringDb :: String -> String -> IO String
readStringDb path name=
    P.readFile ("db/"++path++"/"++name++".json")