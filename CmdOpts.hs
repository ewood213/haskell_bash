module CmdOpts
(
    getCmdOpts,
    Option(..)
) where

import Data.List
import Data.Maybe
import Data.Either

data Option resultType = Option Char String resultType deriving Show
type Arg = String   -- Can be either position or flag
type Flag = String  -- Arg that has a dash in front

-- Parses command line arguments based on Options provided
-- Returns the result types of all flags provided
-- The position arguements
-- And lastly all unmatched flags
getCmdOpts :: [Option a] -> [Arg] -> ([a], [Arg], [Arg])
getCmdOpts options args = (resultTypes, positionalArgs, unmatchedFlags)
    where (flags, positionalArgs) = partition (isPrefixOf "-") args
          (optionMatches, unmatchedFlags) = flagsToOptions options flags
          resultTypes = map (\(Option _ _ c) -> c) $ nubBy optionEqual optionMatches

-- Convert flags to options.
flagsToOptions :: [Option a] -> [Flag] -> ([Option a], [Flag])
flagsToOptions options = partitionEithers . map (parseOption options) . getAllFlags

-- Gets all dash args, namely by applying splitSingleDash
getAllFlags :: [Flag] -> [Flag]
getAllFlags flags = allShortFlags ++ longFlags
    where (longFlags, shortFlags) = partition (isPrefixOf "--") flags
          allShortFlags = concat $ map splitShortFlag shortFlags

-- Find an option that matches the Flag
findOption :: [Option a] -> Flag -> Maybe (Option a)
findOption options flag = find (optionMatches flag) options
        where optionMatches flag (Option c s _) = case length flag of
                                            2 -> c == (flag !! 1)
                                            otherwise -> s == (drop 2 flag)

-- Find an option that matches the flag. If none, return back the arg
parseOption :: [Option a] -> Flag -> Either (Option a) String
parseOption options flag = case findOption options flag of
                            (Just a) -> Left a
                            otherwise -> Right flag

-- take argument -lab and turn into ["-l", "-a", "-b"]
splitShortFlag :: Arg -> [Arg]
splitShortFlag = chunk 2 . (:) '-' . intersperse '-' . tail

-- Test if two options are the same
optionEqual :: Option a -> Option a -> Bool
optionEqual (Option c1 s1 _) (Option c2 s2 _) = c1 == c2 && s1 == s2

-- Chunk into groups
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l = take n l : chunk n (drop n l)