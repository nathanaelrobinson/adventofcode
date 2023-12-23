-- Open and Process Input
import Data.Char (isDigit)
import System.IO

readFileContents :: FilePath -> IO String
readFileContents filePath = do
    -- Open the file for reading
    handle <- openFile filePath ReadMode

    -- Read the contents of the file
    contents <- hGetContents handle

    -- Return the contents
    return contents

processInput ::  String -> [String]
processInput x = lines x

firstDigit :: String -> Maybe Char
firstDigit str = case filter isDigit str of
    [] -> Nothing -- return null if there are no characters
    (x:_) -> Just x -- take the first digit from the list if there are multiple

lastDigit :: String -> Maybe Char
lastDigit str = case filter isDigit str of
    [] -> Nothing
    digits -> Just (last digits)

combineDigits :: String -> Maybe Integer
combineDigits str = do
    first <- firstDigit str
    last <- lastDigit str
    let combinedDigits = read [first, last]
    return combinedDigits

digitsFromStrings :: [String] -> [Maybe Integer]
digitsFromStrings (x:xs) = combineDigits x : digitsFromStrings xs
digitsFromStrings [] = []

mapMaybeToInteger :: Maybe Integer -> Integer
mapMaybeToInteger (Just x) = x
mapMaybeToInteger Nothing  = 0

solve :: String -> Integer
solve a = do
  let strings = processInput a
  let digits = digitsFromStrings strings
  let integers = map mapMaybeToInteger digits
  sum integers

main :: IO ()
main = do
  let inputFile = "input_day1.txt"
  inputString <- readFileContents inputFile
  print (solve inputString)