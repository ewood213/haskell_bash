import Data.List
import System.Environment
import CmdOpts
import System.Directory
import Control.Monad

-- Flags that can be passed to program
options = [ Option 'l' "lines" countLines,
            Option 'w' "words" countWords,
            Option 'c' "chars" countAllChars,
            Option 'L' "max-line-length" maxLineLength ]

-- Helper validation type
data Validation = Success | Error String

-- The default functions to run if None are provided
defaultFns :: [(String -> Int)]
defaultFns = [countLines, countWords, countAllChars]

-- Help dialogue. Not very helpful lol
helpDialogue = "Try wc --help for more information\n"

-- Validates inputs and then runs the word count routine
main :: IO ()
main = do
    args <- getArgs
    let (argFns, fileNames, unmatchedFlags) = getCmdOpts options args
    validation <- verifyInputs fileNames unmatchedFlags
    case validation of
        Error str -> putStr str
        Success -> runWordCount argFns fileNames

-- Runs word count.
-- 1. Gets proper functions and filenames
-- 2. Runs functions
-- 3. IO output
runWordCount :: [(String -> Int)] -> [String] -> IO ()
runWordCount argFns argFiles = do
    allContents <- sequence $ getAllContents argFiles
    let fns = if null argFns then defaultFns else argFns
    let allFiles = getAllFileNames argFiles
    let results = [map ($ content) fns | content <- allContents]
    let allResults = addTotalResult results
    putStr $ resultsToString allResults allFiles

-- Converts [[1, 2], [3, 4]] [thing1.txt, thing2.txt] -> 1 2 thing1.txt
--                                                       3 4 thing2.txt
resultsToString :: [[Int]] -> [String] -> String
resultsToString results fileNames =
    unlines $ map (\x -> resultToString (fst x) (snd x)) (zip results fileNames)
    where resultToString r f = (unwords $ map show r) ++ " " ++ f

-- Verifies that arguments and filenames are valid
-- If not, return an error string
verifyInputs :: [String] -> [String] -> IO (Validation)
verifyInputs fileNames unmatchedFlags = do
    results <- verifyFilenames fileNames
    case results of
        Success -> return $ verifyFlags unmatchedFlags
        Error str -> return $ Error str

-- Verifies files exist
verifyFilenames :: [String] -> IO (Validation)
verifyFilenames fileNames = do
    f <- filterM (fmap not . doesFileExist) fileNames
    case length f of
        0 -> return Success
        _ -> return $ Error $ unlines $ map (++ ": No such file" ) f

-- Gets passed the flags that were unmatched
-- and converts into error string
verifyFlags :: [String] -> Validation
verifyFlags flags =
    case length flags of
        0 -> Success
        _ -> Error $ (unlines $ map ((++) "invalid option: ") flags) ++ "\n" ++ helpDialogue

-- Gets all the filenames
-- If no filenames then it comes from stdin
-- If more than 1 filename, then total is included
getAllFileNames :: [String] -> [String]
getAllFileNames fileNames = case length fileNames of
                                0 -> [""]
                                1 -> fileNames
                                _ -> fileNames ++ ["total"]

-- Adds total result if necessary
addTotalResult :: [[Int]] -> [[Int]]
addTotalResult results = if length results > 1
                         then results ++ [map sum (transpose results)]
                         else results

-- Get all the IO Strings from inputs
getAllContents :: [String] -> [IO String]
getAllContents [] = [getContents]
getAllContents fileNames = map readFile fileNames

countLines :: String -> Int
countLines = count (=='\n')

countWords :: String -> Int
countWords = length . words

maxLineLength :: String -> Int
maxLineLength = maximum . map length . lines

countAllChars :: String -> Int
countAllChars = length

count :: (a -> Bool) -> [a] -> Int
count f = length . filter f