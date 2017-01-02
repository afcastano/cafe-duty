{-# LANGUAGE DeriveGeneric #-}
module App.Service (allPeople, currentDuty, nextDuty, generateRoster) where

import Data.Aeson (FromJSON, ToJSON)
import Data.List
import GHC.Generics

data Person = Person {
    name :: String
  , active :: Bool
  , timesOnDuty :: Int
} deriving ( Generic )

instance ToJSON Person
instance FromJSON Person

bob :: Person
bob = Person { name = "Bob", active = True, timesOnDuty = 0 }

alice :: Person
alice = Person { name = "Alice", active = True, timesOnDuty = 0 }

eve :: Person
eve = Person { name = "Eve", active = True, timesOnDuty = 0 }

peter :: Person
peter = Person { name = "Peter", active = True, timesOnDuty = 0 }

allPeople :: [Person]
allPeople = [bob, alice, eve, peter]

currentDuty :: (Person,Person)
currentDuty = (combine allPeople) !! 0

nextDuty :: (Person,Person)
nextDuty = (combine allPeople) !! 1

generateRoster :: [(Person,Person)]
generateRoster = combine allPeople

--TODO generate all possible combinations. See tuple generator. Check combine 3
combine :: [a] -> [(a,a)]
combine [] = []
combine list = let (l1, l2) = splitInHalf list
                in zip l1 l2

combine3 :: [a] -> [(a,a)]
combine3 [] = []
combine3 list = let (l1, l2) = splitInHalf list
                in [ (x1,x2) | x1 <- l1, x2 <- l2]

splitInHalf :: [a] -> ([a], [a])
splitInHalf [] = ([],[])
splitInHalf list = let splitIndex = (length (list) + 1) `div` 2 
                   in splitAt (splitIndex) list

--Deprecated
combine2 :: Int -> [a] -> [[a]]
combine2 _ [] = []
combine2 0 _ = []
combine2 1 list = map (:[]) list
combine2 size (x:rest) = [x:restCombined | restCombined <- combine2 (size-1) rest] ++ combine2 size rest




