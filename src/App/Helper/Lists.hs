module App.Helper.Lists (splitInHalf, rotate, replaceElem, transformElem, replaceIndex, dropLast) where
import Data.List as L
import Data.Sequence
import Data.Foldable (toList)

splitInHalf :: [a] -> ([a], [a])
splitInHalf [] = ([],[])
splitInHalf list = let splitIndex = (L.length (list)) `div` 2 
                   in L.splitAt (splitIndex) list

replaceIndex :: Int -> a -> [a] -> [a]
replaceIndex index elem list = let seqA = update index elem $ fromList list
                          in toList seqA

replaceElem :: Eq a => a -> a -> [a] -> [a]
replaceElem old newElem list = let maybeIdx = elemIndex old list
                               in case maybeIdx of 
                                    Nothing -> list
                                    Just idx -> replaceIndex idx newElem list

transformElem :: Eq a => (a -> a) -> a -> [a] -> [a]
transformElem mapper elem list = replaceElem elem (mapper elem) list

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate 0 xs = xs
rotate positions xs
    | positions > 0 = L.zipWith const (L.drop positions (L.cycle xs)) xs
    | positions < 0 = let reversed = L.reverse xs
                          rotated  = rotate (abs positions) reversed
                      in L.reverse rotated

dropLast :: Int -> [a] -> [a]
dropLast idx xs = L.take (L.length xs - idx) xs