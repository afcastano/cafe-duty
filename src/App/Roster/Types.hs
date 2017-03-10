{-# LANGUAGE DeriveGeneric #-}
module App.Roster.Types (
            TeamRoster(..)
        ,   current
        ,   next
        ,   addPersonToRoster
        ,   increaseRosterIndex
        ,   decreaseRosterIndex
        ,   replaceInCurrent
        ,   replaceInNext) where

import App.Roster.RosterGeneration (generateNextRoster, generatePreviousRoster)
import App.Helper.Lists (replaceElem)

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

addPersonToRoster :: TeamRoster -> String -> TeamRoster
addPersonToRoster roster personName = case findOddPair $ currentRoster roster of
                                        Just oddPair -> let updatedPair   = replaceEmpty oddPair personName
                                                            updatedRoster = updatePair oddPair updatedPair roster
                                                            newNext       = generateNextRoster $ currentRoster updatedRoster
                                                        in  updatedRoster {nextRoster = newNext}
                                        Nothing      -> let newCurrent    = (currentRoster roster)++[(personName, "")]
                                                            newNext       = generateNextRoster $ newCurrent
                                                        in  roster {currentRoster = newCurrent, nextRoster = newNext}

replaceInCurrent :: TeamRoster -> String -> String -> TeamRoster
replaceInCurrent roster oldName newName
                | oldName == fst (current roster) = let newCurrent = (newName, snd (current roster))
                                                        oldCurrent = current roster
                                                    in updatePair oldCurrent newCurrent roster
                | oldName == snd (current roster) = let newCurrent = (fst (current roster), newName)
                                                        oldCurrent = current roster
                                                    in updatePair oldCurrent newCurrent roster
                | otherwise                    = roster

replaceInNext :: TeamRoster -> String -> String -> TeamRoster
replaceInNext roster oldName newName
                | oldName == fst (next roster) = let newVal = (newName, snd (next roster))
                                                     oldVal = next roster
                                                 in updatePair oldVal newVal roster
                | oldName == snd (next roster) = let newVal = (fst (next roster), newName)
                                                     oldVal = next roster
                                                 in updatePair oldVal newVal roster
                | otherwise                    = roster

next :: TeamRoster -> (String, String)
next roster = let idx = 1 + pairIndex roster
              in getPair idx roster

increaseRosterIndex :: TeamRoster -> TeamRoster
increaseRosterIndex roster
        | (pairIndex roster) + 1 < lengthCurrentRoster roster = roster {pairIndex = (pairIndex roster) + 1}
        | otherwise                                           = calculateNextRoster roster

decreaseRosterIndex :: TeamRoster -> TeamRoster
decreaseRosterIndex roster
        | (pairIndex roster) - 1 >= 0 = roster {pairIndex = (pairIndex roster) - 1}
        | otherwise                   = calculatePreviousRoster roster






-- PRIVATE
updatePair :: (String, String) -> (String, String) -> TeamRoster -> TeamRoster
updatePair old new roster = let newCurrent = replaceElem old new (currentRoster roster)
                                newNext    = replaceElem old new (nextRoster roster)
                            in roster {currentRoster = newCurrent, nextRoster = newNext}

calculateNextRoster :: TeamRoster -> TeamRoster
calculateNextRoster roster = let newCurrent = nextRoster roster
                                 newNext    = generateNextRoster newCurrent
                             in roster {currentRoster = newCurrent, nextRoster = newNext, pairIndex = 0}

calculatePreviousRoster :: TeamRoster -> TeamRoster
calculatePreviousRoster roster = let newNext    = currentRoster roster
                                     newCurrent = generatePreviousRoster $ newNext
                                     newIdx     = (lengthCurrentRoster roster) - 1
                                 in roster {currentRoster = newCurrent, nextRoster = newNext, pairIndex = newIdx}


getPair :: Int -> TeamRoster -> (String, String)
getPair idx roster
        | idx <  lengthCurrentRoster roster = (safeCurrentRoster roster) !! idx  -- TODO dont use !!. It's risky.
        | otherwise                         = (safeNextRoster roster) !! (idx - (lengthCurrentRoster roster))


lengthCurrentRoster :: TeamRoster -> Int
lengthCurrentRoster roster = length (safeCurrentRoster roster)

filterOutEmpty :: [(String, String)] -> [(String, String)]
filterOutEmpty list = let nonEmpty (s1, s2) = s1 /= "" && s2 /= ""
                      in filter nonEmpty list

findOddPair :: [(String, String)] -> Maybe (String, String)
findOddPair list = let oddPair (s1, s2) = s1 == "" || s2 == ""
                   in find oddPair list

-- Current roster with no empty tuples
safeCurrentRoster :: TeamRoster -> [(String, String)]
safeCurrentRoster roster = filterOutEmpty $ currentRoster roster

safeNextRoster :: TeamRoster -> [(String, String)]
safeNextRoster roster = filterOutEmpty $ nextRoster roster

replaceEmpty :: (String, String) -> String -> (String, String)
replaceEmpty pair pName
            | "" == fst pair = (pName, snd pair)
            | "" == snd pair = ((fst pair), pName)
            | otherwise      = pair
