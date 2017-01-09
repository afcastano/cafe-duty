module App.Roster.RosterGeneration (combineElements) where
import App.Helper.Lists

-- Roster generator algorithm. Can be improved a lot!
combineElements :: [a] -> [[a]]
combineElements []   = []
combineElements list = let (l1, l2) = splitInHalf list
                           pairs = zipAndRotate 0 l1 l2
                       in  map pairToList pairs


pairToList :: (a,a) -> [a]
pairToList (fst, snd) = [fst, snd]

zipAndRotate :: Int -> [a] -> [a] -> [(a,a)]
zipAndRotate positions l1 l2
    | positions >= length l2  = []
    | otherwise               = (zip l1 (rotate positions l2)) ++ zipAndRotate (positions+1) l1 l2