module Commands.User (doUserCommand) where

import Commands
import Config
import qualified Dictionary  as D


-- #### Defines ####

type UserFunc = [String] -> IO String


-- #### Main ####

-- If the message is longer than 2 chars (after whitespace has been stripped)
-- and begins with either 'commandPrefix' or 'dictionaryPrefix' then interpret
-- as a command, else ignore.
doUserCommand :: String -> IO String
doUserCommand xs@(y:ys)
  | length (stripWhitespace xs) < 2 = return ""
  | y == commandPrefix    = guard $ runUserCommand ys
  | y == dictionaryPrefix = guard $ D.dictionaryLookup ys
  | otherwise = return ""
    where guard = checkCharacters ys

runUserCommand :: String -> IO String
runUserCommand x = let args = tail (words x)
                       func = lookupCommand userCommands $ head (words x) in
  case func of
    (Just f) -> f args
    Nothing  -> return $ "Error: command '" ++ x ++ "' not found"


-- #### User Commands ####

echo :: UserFunc
echo [] = return wrongNumArgs
echo xs = return $ unwords xs

dictionaryList :: UserFunc
dictionaryList [] = D.dictionaryList
dictionaryList xs = return wrongNumArgs

dictionaryUpdate :: UserFunc
dictionaryUpdate (x:y:z) = D.dictionaryUpdate x (unwords $ tail y:z)
dictionaryUpdate _ = return wrongNumArgs

dictionaryRemove :: UserFunc
dictionaryRemove (x:[]) = D.dictionaryRemove x
dictionaryRemove _ = return wrongNumArgs

userCommands :: [ (CommandName, UserFunc, CommandHelp) ]
userCommands = 
  [ ("help", return . (help userCommands),
     "[command-name] -- if no argument given, prints list of user commands, else prints help for specific user command")
  , ("echo", echo,
     "<argument>[...] -- prints all given argument(s)")
  , ("dictionary-list", dictionaryList,
     "-- lists all dictionary entries")
  , ("dictionary-update", dictionaryUpdate,
     "<key> <definition>[...] -- adds or updates a word and its definition in the dictionary")
  , ("dictionary-remove", dictionaryRemove,
     "<key> -- removes a word and its definition from the dictionary") ]
