module Config where

import System.IO
import Data.ConfigFile
import Data.Either.Utils

import IRC
import Network


configDir     = "config/"
dictionaryDir = configDir ++ "dictionary/"
configFile    = configDir ++ "config"

-- used by users to run a command or get the value from the dictionary
commandPrefix    = '!'
dictionaryPrefix = '@'

-- used in the log
userMsgDestination = " -> "
userMsgPrefix = ": "
userActPrefix = "* "
chanMsgPrefix = "@ "
servMsgPrefix = "# "

debugIn  = "[DEBUG] <- "
debugOut = "[DEBUG] -> "

-- Setting this to true makes hbot print out the exact strings sent and
-- received from the server.
debug = True

-- "Thus, there are 510 characters maximum allowed for the command and its parameters."
-- http://tools.ietf.org/html/rfc2812
maxMsgLen :: Int
maxMsgLen = 510


-- #### Main ####

getConfig :: IO IRC
getConfig = do
  file <- fmap forceEither $ readfile emptyCP configFile
  let getKey key = forceEither $ get file "" key
  return $ IRC (getKey "server")
               (PortNumber $ toEnum $ getKey "port") -- wtf!? why is PortNumber not Int -> PortID?
               (words $ getKey "channels")
               (getKey "nickname")
               (getKey "username")
               (getKey "realname")
               (getKey "password")
               "111.111.111.111" -- will be automatically set on connection
