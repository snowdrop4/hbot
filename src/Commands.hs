module Commands where

import Data.Char
import Data.List
import Data.Maybe
import Data.Tuple.Utils


-- #### Defines ####

type CommandName = String
type CommandHelp = String

wrongNumArgs = "Error: incorrect number of arguments"


-- #### Util ####

stripWhitespace :: String -> String
stripWhitespace = concat . words

illegalCharacters :: String -> Bool
illegalCharacters = isJust . find (\x -> (not . isAlphaNum) x && x /= '-' && x /= '_')

checkCharacters :: String -> IO String -> IO String
checkCharacters xs f = if illegalCharacters $ head (words xs)
  then return "Error: only alphanumeric, '_', and '-' characters allowed in command or dictionary names"
  else f

lookupCommand :: [(String, b, c)] -> String -> Maybe b
lookupCommand commList x = lookup x $ map (\(k, f, _) -> (k, f)) commList

listCommands :: [(String, b, c)] -> String
listCommands commList = "Available commands: " ++ intercalate ", " (map fst3 commList)

lookupHelp :: [(String, b, String)] -> String -> Maybe String
lookupHelp commList x = lookup x $ map (\(k, _, h) -> (k, "Help: " ++ k ++ " " ++ h)) commList

help :: [(String, a, String)] -> [String] -> String
help commList []     = listCommands commList
help commList (x:[]) = fromMaybe ("Error: could not find help for command '" ++ x ++ "'") (lookupHelp commList x)
help commList (x:xs) = wrongNumArgs
