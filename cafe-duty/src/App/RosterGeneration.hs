module App.RosterGeneration (generateRoster) where
import App.Helper.Lists

-- Roster generator algorithm
generateRoster :: [a] -> [(a,a)]
generateRoster []   = []
generateRoster list = let (l1, l2) = splitInHalf list
                      in zipAndRotate 0 l1 l2

zipAndRotate :: Int -> [a] -> [a] -> [(a,a)]
zipAndRotate positions l1 l2
    | positions >= length l2  = []
    | otherwise               = (zip l1 (rotate positions l2)) ++ zipAndRotate (positions+1) l1 l2