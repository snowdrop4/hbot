module Dictionary
    ( dictionaryList
    , dictionaryLookup
    , dictionaryUpdate
    , dictionaryRemove ) where

import Prelude         hiding (catch, readFile, writeFile)
import System.IO.Error hiding (catch)
import System.IO
import System.Directory
import Control.Exception
import Data.List
import Data.String.Utils

import Config


-- #### Util ####

fileHandler :: IO a -> a -> IO a
fileHandler x err = x `catch` handleExists
  where handleExists e
          | isDoesNotExistError e = return err
          | otherwise = throwIO e


-- #### Main ####

dictionaryList :: IO String
dictionaryList = fmap (("Available words: " ++) . intercalate ", " . filter (\x -> x /= ".." && x /= "."))
                       (getDirectoryContents dictionaryDir)

dictionaryLookup :: String -> IO String
dictionaryLookup [] = return "Error: <key> can't be blank"
dictionaryLookup xs = fmap (replace "\\n" "\n" . unwords . words) -- remove excess spaces, add newlines if (literal) "\n" in text
                        (readFile (dictionaryDir ++ xs) `fileHandler` "Error: dictionary entry not found")

dictionaryUpdate :: String -> String -> IO String
dictionaryUpdate [] _ = return "Error: <key> can't be blank"
dictionaryUpdate _ [] = return "Error: <definition>[...] can't be blank"
dictionaryUpdate xs newText = writeFile (dictionaryDir ++ xs) newText >> return
                  "Success: dictionary entry updated" `fileHandler`
                  "Error: dictionary entry could not be updated"

dictionaryRemove :: String -> IO String
dictionaryRemove [] = return "Error: <key> can't be blank"
dictionaryRemove xs = removeFile (dictionaryDir ++ xs) >> return
                  "Success: dictionary entry removed" `fileHandler`
                  "Error: dictionary entry could not be found"
