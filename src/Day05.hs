{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day05(d05p01, d05p02) where

import Data.List (sortOn)
import Data.List.Split (chunksOf, splitWhen)
import Data.Maybe (listToMaybe, mapMaybe)
import qualified Data.Range as R
import Text.Read (readMaybe)
import Utils (readLines)

data Range = Range
  { des :: Int,
    src :: Int,
    len :: Int
  }
  deriving (Show, Eq, Ord)

data FiniteRange = FiniteRange
  { ini :: Int,
    end :: Int
  }
  deriving (Eq, Show, Ord)

data FiniteRangeMapping = FiniteRangeMapping
  { fDes :: FiniteRange,
    fSrc :: FiniteRange
  }
  deriving (Eq, Show, Ord)

newtype FiniteRangeMap = FiniteRangeMap
  { fRanges :: [FiniteRangeMapping]
  }
  deriving (Eq, Show, Ord)

singleFiniteRange :: Int -> FiniteRange
singleFiniteRange n = FiniteRange {ini = n, end = n}

toRange :: [Int] -> Range
toRange ls = Range (head ls) (ls !! 1) (ls !! 2)

fromRange :: Range -> FiniteRangeMapping
fromRange r = FiniteRangeMapping {fDes = FiniteRange {ini = des r, end = len r + des r}, fSrc = FiniteRange {ini = src r, end = len r + src r}}

parseSeedFRange :: [Int] -> FiniteRange
parseSeedFRange r = FiniteRange {ini = head r, end = head r + (r !! 1) - 1}

parseNumbers :: String -> [Int]
parseNumbers = mapMaybe readMaybe . words

parseSeeds :: [String] -> [Int]
parseSeeds = concatMap parseNumbers . listToMaybe

parseSeedFRanges1 :: [String] -> [FiniteRange]
parseSeedFRanges1 = map singleFiniteRange . parseSeeds

parseFMappings :: [String] -> [FiniteRangeMap]
parseFMappings = map (FiniteRangeMap . (map (fromRange . toRange . parseNumbers) . drop 1)) . splitWhen (== "") . drop 2

fmLen :: FiniteRangeMapping -> Int
fmLen m = (end . fDes) m - (ini . fDes) m

fmDis :: FiniteRangeMapping -> Int
fmDis fm = (ini . fDes) fm - (ini . fSrc) fm

rHead :: R.Range Int -> Int
rHead (R.SpanRange (R.Bound i R.Inclusive) _) = i
rHead (R.SpanRange (R.Bound i R.Exclusive) _) = i + 1
rHead (R.SingletonRange i) = i
rHead _ = error "Not Finite Range"

rLast :: R.Range Int -> Int
rLast (R.SpanRange _ (R.Bound e R.Inclusive)) = e
rLast (R.SpanRange _ (R.Bound e R.Exclusive)) = e - 1
rLast (R.SingletonRange e) = e
rLast _ = error "Not Finite Range"

frsHead :: [FiniteRange] -> Int
frsHead = ini . head . sortOn ini

toDRange :: FiniteRange -> R.Range Int
toDRange fr = ini fr R.+=+ end fr

toDRanges :: [FiniteRange] -> [R.Range Int]
toDRanges = R.joinRanges . R.mergeRanges . map toDRange

toFRange :: R.Range Int -> FiniteRange
toFRange r = FiniteRange {ini = rHead r, end = rLast r}

toFRanges :: [R.Range Int] -> [FiniteRange]
toFRanges = map toFRange . R.joinRanges . R.mergeRanges

mergeFRanges :: [FiniteRange] -> [FiniteRange]
mergeFRanges = toFRanges . toDRanges

frsIntersec :: [FiniteRange] -> [FiniteRange] -> [FiniteRange]
frsIntersec frs1 frs2 = mergeFRanges . toFRanges $ R.intersection (map toDRange frs1) (map toDRange frs2)

frsDiff :: [FiniteRange] -> [FiniteRange] -> [FiniteRange]
frsDiff frs1 frs2 = mergeFRanges . toFRanges $ R.difference (map toDRange frs1) (map toDRange frs2)

frPlusDis :: Int -> FiniteRange -> FiniteRange
frPlusDis d (FiniteRange {ini = i, end = e}) = FiniteRange {ini = i + d, end = e + d}

mapWithFiniteRangeMapping :: [FiniteRange] -> FiniteRangeMapping -> [FiniteRange]
mapWithFiniteRangeMapping frs frmp = map (frPlusDis (fmDis frmp)) (frsIntersec [fSrc frmp] frs)

fSrcs :: FiniteRangeMap -> [FiniteRange]
fSrcs = mergeFRanges . map fSrc . fRanges

mapWithFiniteRangeMap :: [FiniteRange] -> FiniteRangeMap -> [FiniteRange]
mapWithFiniteRangeMap frs frm = mergeFRanges $ concatMap (mapWithFiniteRangeMapping frs) (fRanges frm) ++ frsDiff frs (fSrcs frm)

mapWithFiniteRangeMaps :: [FiniteRange] -> [FiniteRangeMap] -> [FiniteRange]
mapWithFiniteRangeMaps = foldl mapWithFiniteRangeMap

exec1 :: [String] -> Int
exec1 ss = frsHead $ mapWithFiniteRangeMaps (parseSeedFRanges1 ss) (parseFMappings ss)

d05p01 :: IO ()
d05p01 = do
  contents <- readLines "input/d05.txt"
  print "Day 05 - Part 1:"
  print $ exec1 contents

parseSeedFRanges2 :: [String] -> [FiniteRange]
parseSeedFRanges2 = map parseSeedFRange . chunksOf 2 . parseSeeds

d05p02 :: IO ()
d05p02 = do
  contents <- readLines "input/d05.txt"
  print "Day 05 - Part 2:"
  print $ exec2 contents

exec2 :: [String] -> Int
exec2 ss = frsHead $ mapWithFiniteRangeMaps (parseSeedFRanges2 ss) (parseFMappings ss)
