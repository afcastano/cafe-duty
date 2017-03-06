module App.Roster.RosterGeneration (generateInitialRoster, generateNextRoster) where
import App.Helper.Lists(splitInHalf, replaceIndex, rotate)

-- Roster generation algorthm based on:
-- https://en.wikipedia.org/wiki/Round-robin_tournament#Scheduling_algorithm
-- http://stackoverflow.com/questions/41896889/algorithm-to-schedule-people-to-an-activity-that-should-be-done-in-pairs


-- Empty value is provided in case of an odd number of elements.
generateInitialRoster :: [a] -> a -> [(a,a)]
generateInitialRoster list emptyValue
                | isOdd $ length list = getRosterFromList $ list ++ [emptyValue]
                | otherwise           = getRosterFromList list

-- Given the current roster, generates another permutation.
generateNextRoster :: [(a,a)] -> [(a,a)]                                  -- given (1,4)(2,5)(3,6)
generateNextRoster roster = let (l1, l2)       = unzip roster             -- [1,2,3][4,5,6]
                                head:xs        = l1 ++ (reverse l2)       -- 1 : [2,3,6,5,4]
                                rotatedList    = head:(rotate 1 xs)       -- [1,3,6,5,4,2]
                                (newL1, newL2) = splitInHalf rotatedList  -- ([1,3,6],[5,4,2])
                            in  zip newL1 (reverse newL2)                 -- (1,2)(3,4)(6,5)

-- Given the current roster, generates the previous permutation.
generatePreviousRoster :: [(a,a)] -> [(a,a)]                                  -- given (1,4)(2,5)(3,6)
generatePreviousRoster roster = let (l1, l2)       = unzip roster             -- [1,2,3][4,5,6]
                                    head:xs        = l1 ++ (reverse l2)       -- 1 : [2,3,6,5,4]
                                    rotatedList    = head:(rotate (-1) xs)    -- [1,3,6,5,4,2]
                                    (newL1, newL2) = splitInHalf rotatedList  -- ([1,3,6],[5,4,2])
                                in  zip newL1 (reverse newL2)                 -- (1,2)(3,4)(6,5)



-- private functions
getRosterFromList :: [a] -> [(a,a)]
getRosterFromList list = let (l1, l2) = splitInHalf list
                         in zip l1 l2

isOdd :: Int -> Bool
isOdd n = n `mod` 2 == 1