module Utils( readLines, multiMatchRegex ) where
import Text.RegexPR (multiMatchRegexPR)
import Data.Maybe (mapMaybe, listToMaybe)

readLines :: FilePath -> IO [String]
readLines = fmap lines . readFile

multiMatchRegex :: String -> String -> [String]
multiMatchRegex p s = map snd (mapMaybe (listToMaybe . snd) (multiMatchRegexPR p s))