{-# OPTIONS_GHC -Wno-missing-export-lists #-}
{-# OPTIONS_GHC -Wno-unused-matches #-}

module Day06 where

import Control.Monad (ap)
import Data.Maybe (mapMaybe, maybeToList)
import Text.Read (readMaybe)
import Utils (readLines)

d06p01 :: IO ()
d06p01 = do
  contents <- readLines "input/d06.txt"
  print "Day 06 - Part 1:"
  print $ e1 contents

data Race = Race
  { time :: Int,
    distance :: Int
  }
  deriving (Show, Eq, Ord)

toRace :: (Int, Int) -> Race
toRace (x, y) = Race {time = x, distance = y}

numbers :: String -> [Int]
numbers = mapMaybe readMaybe . drop 1 . words

parseRaces1 :: [String] -> [Race]
parseRaces1 ss = zipWith (curry toRace) ((numbers . head) ss) ((numbers . last) ss)

calc :: (Num a) => a -> a -> a
calc t s = (t - s) * s

secs :: Race -> [Int]
secs r = [(time r `div` 2), (time r `div` 2) - 1 .. 1]

seconds :: Race -> [Int]
seconds = ap (map . calc . time) secs

over :: Race -> [Int] -> [Int]
over = takeWhile . (<) . distance

winners :: Race -> [Int]
winners = ap over seconds

winnersN :: Race -> Int
winnersN r
  | odd (time r) = (2*) . length . winners $ r
  | otherwise = ((2*) . length . winners) r -1


e1 :: [String] -> Int
e1 = product . map winnersN . parseRaces1

d06p02 :: IO ()
d06p02 = do
  contents <- readLines "input/d06.txt"
  print "Day 06 - Part 2:"
  print  "Too long: 39570185"

number :: String -> Int
number = head . maybeToList . readMaybe . concat . drop 1 . words

parseRaces2 :: [String] -> Race
parseRaces2 ss = toRace ((number . head) ss, (number . last) ss)

e2 :: [String] -> Int
e2 = winnersN . parseRaces2