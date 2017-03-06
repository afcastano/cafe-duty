{-# LANGUAGE DeriveGeneric #-}
module App.Roster.Types ( TeamRoster(..), current, next, updateToNextDay) where

import App.Roster.RosterGeneration (generateNextRoster)

import Data.Aeson (FromJSON, ToJSON)
import Data.List
import GHC.Generics

data TeamRoster = TeamRoster {
      teamName      :: String
    , currentRoster :: [(String,String)]
    , nextRoster    :: [(String,String)]
    , pairIndex     :: Int
} deriving ( Generic, Show )

instance ToJSON TeamRoster
instance FromJSON TeamRoster
instance Eq TeamRoster where
  TeamRoster n1 _ _ _ == TeamRoster n2 _ _ _ = n1 == n2

-- Functions on TeamRoster

current :: TeamRoster -> (String, String)
current roster = let pairs = safeCurrentRoster roster
                     idx   = pairIndex roster
                 in  pairs !! idx -- TODO dont use !!. It's risky.

next :: TeamRoster -> (String, String)
next roster = let idx = 1 + pairIndex roster
              in getPair idx roster

updateToNextDay :: TeamRoster -> TeamRoster
updateToNextDay roster
        | (pairIndex roster) + 1 < lengthCurrentRoster roster = roster {pairIndex = (pairIndex roster) + 1}
        | otherwise                                           = generateNewRoster roster

-- PRIVATE
generateNewRoster :: TeamRoster -> TeamRoster
generateNewRoster roster = let newCurrent = nextRoster roster
                               newNext = generateNextRoster newCurrent
                           in roster {currentRoster = newCurrent, nextRoster = newNext, pairIndex = 0}

getPair :: Int -> TeamRoster -> (String, String)
getPair idx roster
        | idx <  lengthCurrentRoster roster = (safeCurrentRoster roster) !! idx  -- TODO dont use !!. It's risky.
        | otherwise                         = (safeNextRoster roster) !! (idx - (lengthCurrentRoster roster))


lengthCurrentRoster :: TeamRoster -> Int
lengthCurrentRoster roster = length (safeCurrentRoster roster)

filterOutEmpty :: [(String, String)] -> [(String, String)]
filterOutEmpty list = let nonEmpty (s1, s2) = s1 /= "" && s2 /= ""
                      in filter nonEmpty list

-- Current roster with no empty tuples
safeCurrentRoster :: TeamRoster -> [(String, String)]
safeCurrentRoster roster = filterOutEmpty $ currentRoster roster

safeNextRoster :: TeamRoster -> [(String, String)]
safeNextRoster roster = filterOutEmpty $ nextRoster roster

