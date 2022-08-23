module IRC.Network where

import Prelude hiding (readFile, writeFile)
import System.IO
import Data.List
import Data.Monoid
import Control.Arrow
import Control.Monad
import qualified Data.ByteString      as BSW8
import qualified Data.ByteString.UTF8 as BSU8

import IRC
import Config
import Print


-- Send message to server
write :: Handle -> String -> String -> IO ()
write h s t = nPrint h msg
  where msg = s ++ " " ++ t ++ "\r\n"

writeBS :: Handle -> BSW8.ByteString -> BSW8.ByteString -> IO ()
writeBS h s t = nPrintBS h msg
  where msg = s <> BSU8.fromString " " <> t <> BSU8.fromString "\r\n"

-- Must be used before 'setName' or 'setNick' for some reason...
setPass :: Handle -> String -> IO ()
setPass h = write h "PASS"

setName :: Handle -> String -> String -> IO ()
setName h username realname =
  write h "USER" $ username ++ " 0 * :" ++ realname

setNick :: Handle -> String -> IO ()
setNick h = write h "NICK"

getHost :: Handle -> String -> IO ()
getHost h = write h "WHOIS"

join :: Handle -> String -> IO ()
join h = write h "JOIN"

leave :: Handle -> String -> IO ()
leave h = write h "PART"

quit :: Handle -> String -> IO ()
quit h reason = write h "QUIT" $ ':' : reason

kick :: Handle -> String -> String -> String -> IO ()
kick h chan nick msg = write h "KICK" (chan ++ " " ++ nick ++ " :" ++ msg)

mode :: Handle -> String -> String -> String -> IO ()
mode h set flags args = write h "MODE" (set ++ " " ++ flags ++ " " ++ args)

-- sendMsg:
-- * Splits long messages up. This requires that the hostname is set correctly
--   in the IRC data structure.
-- * Sends a seperate message for each line in the given string.
-- * If the given string starts with "/me " send the ACTION command (by
--   forwarding it on to 'sendActMsg')
sendMsg :: Server -> String -> String -> IO ()
sendMsg _ _ [] = return ()
sendMsg c@(Server i h) recipient msg = do
  -- send the first part
  if "/me " `isPrefixOf` first
    then sendActMsg c recipient (drop (length "/me ") first)
    else write h "PRIVMSG" $ recipient ++ " :" ++ first
  -- send the second part
  sendMsg c recipient rest
    where (firstLine, _) = second (drop 1) $ break (== '\n') msg 
          (first,  rest) = splitAt msgLen firstLine
          msgLen = maxMsgLen - length(":"  ++ botNick i ++
                                      "!~" ++ botUser i ++
                                      "@"  ++ botHost i ++
                                      " PRIVMSG " ++ recipient ++ " :")

sendActMsg :: Server -> String -> String -> IO ()
sendActMsg _ _ [] = return ()
sendActMsg c@(Server i h) recipient msg =
  writeBS h (BSU8.fromString "PRIVMSG")
            (BSU8.fromString (recipient ++ " :") <> m2)
    where m1 = BSU8.fromString ("ACTION " ++ msg)
          m2 = (1 `BSW8.cons` m1) `BSW8.snoc` 1
