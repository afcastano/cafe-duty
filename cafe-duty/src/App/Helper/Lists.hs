module App.Helper.Lists (splitInHalf, rotate) where
import Data.List

splitInHalf :: [a] -> ([a], [a])
splitInHalf [] = ([],[])
splitInHalf list = let splitIndex = (length (list) + 1) `div` 2 
                   in splitAt (splitIndex) list

rotate :: Int -> [a] -> [a]
rotate _ [] = []
rotate positions xs = zipWith const (drop positions (cycle xs)) xs