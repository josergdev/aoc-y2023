module Utils(printList, readLines, multiMatchRegex, matchRegex, matchRegexToInt, section) where

import Text.RegexPR (multiMatchRegexPR, matchRegexPR)
import Data.Maybe (mapMaybe, listToMaybe)
import Text.Read (readMaybe)

printList :: (Foldable t, Show a) => IO (t a) -> IO ()
printList i = mapM_ print =<< i

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

multiMatchRegex :: String -> String -> [String]
multiMatchRegex p s = map snd (mapMaybe (listToMaybe . snd) (multiMatchRegexPR p s))

matchRegex :: String -> String -> Maybe String
matchRegex p l = fmap snd ((listToMaybe . snd) =<< matchRegexPR p l)

matchRegexToInt :: String -> String -> Maybe Int
matchRegexToInt p l = readMaybe =<< matchRegex p l

section :: b -> a -> (a, b)
section = flip (,)
