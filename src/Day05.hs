{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day05 where

import Utils (readLines)
import Data.Maybe (listToMaybe, mapMaybe, catMaybes)
import Data.List ( find )
import Text.Read (readMaybe)
import Data.List.Split (splitOn, splitWhen)
import Control.Monad (liftM2)

d05p01 :: IO ()
d05p01 = do
  contents <- readLines "input/d05.txt"
  print "Day 05 - Part 1:"
  print $ exec1 contents


data Range = Range {des :: Int, src :: Int, len :: Int} deriving (Show, Eq, Ord)

toRange :: [Int] -> Range
toRange ls = Range (head ls) (ls !! 1) (ls !! 2)

newtype RangeMap = RangeMap {ranges :: [Range]} deriving (Show, Eq, Ord)

mapNumberRange :: Range -> Int -> Int
mapNumberRange r n = des r - src r + n

findRange :: RangeMap -> Int -> Maybe Range
findRange rm n = find (\r -> (n >= src r) && (n < (len r + src r)) ) (ranges rm)

mapNumber :: RangeMap -> Int -> Int
mapNumber rm n = maybe n (`mapNumberRange` n) (findRange rm n)

parseNumbers :: String -> [Int]
parseNumbers = mapMaybe readMaybe . words

parseSeeds :: [String] -> [Int]
parseSeeds = concatMap parseNumbers . listToMaybe

parseMaps :: [String] -> [RangeMap]
parseMaps = map (RangeMap . (map (toRange . parseNumbers) . drop 1)) . splitWhen (=="") . drop 2

computeMapping :: [RangeMap] -> Int -> Int
computeMapping rms s = foldl (flip mapNumber) s rms

computeMappingN :: [RangeMap] -> [Int] -> [Int]
computeMappingN rms = map (computeMapping rms)

collect1 :: [String] -> Int
collect1 = minimum . liftM2 computeMappingN parseMaps parseSeeds

exec1 :: [String] -> Int
exec1 = collect1