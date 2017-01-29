module App.Roster.RosterGeneration (combineElements, generateInitialRoster, generateNextRoster) where
import App.Helper.Lists(splitInHalf, replaceIndex, rotate)

-- Roster generation algorthm based on:
-- https://en.wikipedia.org/wiki/Round-robin_tournament#Scheduling_algorithm
-- http://stackoverflow.com/questions/41896889/algorithm-to-schedule-people-to-an-activity-that-should-be-done-in-pairs


-- Empty value is provided in case of an odd number of elements.
generateInitialRoster :: [a] -> a -> [(a,a)]
generateInitialRoster list emptyValue
                | isOdd $ length list = rosterOfList $ list ++ [emptyValue]
                | otherwise           = rosterOfList list

-- Given the current roster, generates another permutation.
generateNextRoster :: [(a,a)] -> [(a,a)]                                  -- given (1,4)(2,5)(3,6)
generateNextRoster roster = let (l1, l2)       = unzip roster             -- [1,2,3][4,5,6]
                                head:xs        = l1 ++ (reverse l2)       -- 1 : [2,3,6,5,4]
                                rotatedList    = head:(rotate 1 xs)       -- [1,3,6,5,4,2]
                                (newL1, newL2) = splitInHalf rotatedList  -- ([1,3,6],[5,4,2])
                            in  zip newL1 (reverse newL2)                 -- (1,2)(3,4)(6,5)



-- private functions
rosterOfList :: [a] -> [(a,a)]
rosterOfList list = let (l1, l2) = splitInHalf list
                    in zip l1 l2

isOdd :: Int -> Bool
isOdd n = n `mod` 2 == 1

combineElements :: [a] -> [[a]]
combineElements []   = []
combineElements list = let (l1, l2) = splitInHalf list
                       in  swapAndCombine (-1) l1 l2
                            

swapAndCombine :: Int -> [a] -> [a] -> [[a]]
swapAndCombine _ [] _ = []
swapAndCombine _ _ [] = []
swapAndCombine idxToSwap l1 l2
    | idxToSwap == -1           = combine l1 l2 ++ (swapAndCombine (idxToSwap + 1) l1 l2)
    | idxToSwap >= length l1    = []
    | otherwise                 = let (s1, s2) = swapAtIdx idxToSwap l1 l2
                                  in combine s1 s2 ++ (swapAndCombine (idxToSwap + 1) s1 s2)


swapAtIdx :: Int -> [a] -> [a] -> ([a], [a])
swapAtIdx idx l1 l2 = let rotatedL2 = rotate (length l2 - 1) l2
                          elemL1    = l1!!idx
                          elemL2    = rotatedL2!!idx
                          newL1     = replaceIndex idx elemL2 l1
                          newL2     = rotate 1 $ replaceIndex idx elemL1 rotatedL2
                      in (newL1, newL2)

combine :: [a] -> [a] -> [[a]]
combine l1 l2 = let pairs = zipAndRotate 0 l1 l2
                in  map pairToList pairs

pairToList :: (a,a) -> [a]
pairToList (fst, snd) = [fst, snd]

zipAndRotate :: Int -> [a] -> [a] -> [(a,a)]
zipAndRotate positions l1 l2
    | positions >= length l2  = []
    | otherwise               = (zip l1 (rotate positions l2)) ++ zipAndRotate (positions+1) l1 l2