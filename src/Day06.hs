{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Day06 where
import Utils (readLines)

d06p01 :: IO ()
d06p01 = do
  contents <- readLines "input/d06.txt"
  print "Day 06 - Part 1:"
  print $ exec1 contents

exec1 ss = ss

d06p02 :: IO ()
d06p02 = do
  contents <- readLines "input/d06.txt"
  print "Day 06 - Part 2:"
  print $ exec2 contents

exec2 ss = ss