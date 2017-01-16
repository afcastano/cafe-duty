{-# LANGUAGE DeriveDataTypeable #-}
module App.TeamPageService (getNewTeamPage) where

import Text.Hastache
import Text.Hastache.Context
import qualified Data.Text.Lazy.IO as TL
import Data.Text.Lazy

import App.Roster.Types(Team(..), Person(..))

---- UI specific stuff.
--- new team
getNewTeamPage :: IO Text
getNewTeamPage = do
              let context "name" = MuVariable $ "Create" -- DUH????
              useTemplate "templates/new_team.html" context


useTemplate :: String -> (String -> MuType IO) -> IO Text
useTemplate templateName context = hastacheFile defaultConfig templateName (mkStrContext context)
