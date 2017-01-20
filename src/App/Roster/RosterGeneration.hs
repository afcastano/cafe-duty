module App.Roster.RosterGeneration (combineElements) where
import App.Helper.Lists(splitInHalf, replaceIndex, rotate)

-- Roster generator algorithm. Super complicated and undocumented! Can be improved a lot

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