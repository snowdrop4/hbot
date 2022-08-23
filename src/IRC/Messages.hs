module IRC.Messages (parseMsg, replyMsg, Msg(..)) where

import Data.List
import Control.Monad

import IRC
import IRC.Network
import Commands.User
import Web
import Config
import Print


-- #### Defines ####

data Msg 
  = UserMsg   { from :: String, to    :: String, msg :: String }
  | UserNick  { from :: String, to    :: String }
  | UserJoin  { user :: String, chan  :: String }
  | UserPart  { user :: String, chan  :: String }
  | UserQuit  { user :: String, msg   :: String }
  | Kick      { from :: String, chan  :: String, user :: String, msg :: String }
  | UserMode  { from :: String, to    :: String, mode :: String }
  | UserHost  { user :: String, host  :: String   }
  | ChanTopic { chan :: String, topic :: String   }
  | ChanNicks { chan :: String, nicks :: [String] }
  | ServMsg   { from :: String, msg   :: String   }
  | Ping      { from :: String }
  | Ignored
  | Unknown   { contents :: String }

msgTypeServMsg =
  [ "NOTICE" -- <Server Notices>  :<server> NOTICE * :*** Looking up your hostname...
  , "001"    -- <Welcome Msg>     :<server> 001 <nick> :Welcome to the freenode Internet Relay Chat Network <nickname>
  , "002"    -- <Server Details>  :<server> 002 <nick> :Your host is <server>[<ip>], running version <software and version>
  , "250"    -- <Max Users>       :<server> 250 <nick> :Highest Server count: 8595 (8594 clients) (4601310 Servers received)
  , "251"    -- <Connected Users> :<server> 251 <nick> :There are 223 users and 80659 invisible on 33 servers
  , "255"    -- <Server Clients>  :<server> 255 <nick> :I have 3849 clients and 1 servers
  , "265"    -- <Local Users>     :<server> 265 <nick> <current> <max> :Current local users 3849, max 8594
  , "266"    -- <Global Users>    :<server> 266 <nick> <current> <max> :Current global users 77421, max 88420
  , "372"    -- <MOTD>            :<server> 372 <nick> :<MOTD>
  , "375"    -- <MOTD>            :<server> 375 <nick> :<MOTD>
  , "401"    -- <Server  !Exist>  :<server> 403 <server> :No such server
  , "402"    -- <Nick    !Exist>  :<server> 403 <nick> :No such nick
  , "403"    -- <Channel !Exist>  :<server> 403 <chan> :No such channel
  , "442"    -- <Not on Channel>  :<server> 442 <nick> <chan> :You're not on that channel
  ]

msgTypeIgnore = 
  [ "003"    -- <Server Create Time> :<server> 003 <nick> :This server was created <date> at <time> <timezone>
  , "004"    -- <Server Details>     :<server> 004 <nick> <server> <irrelevent_server_details>
  , "005"    -- <MAP>                :<server> 005 <nick> <huge_load_of_bullshit> :are supported by this server
  , "007"    -- <MAP - End>          :<server> 007 <nick> :End of /MAP
  , "008"    -- <Server Notice Mask> (?)
  , "252"    -- <IRC OPs Online>     :<server> 252 <operator_num> :IRC operators online
  , "253"    -- <Attempted Logins>   :<server> 253 <Server_num> :unknown Server(s)
  , "254"    -- <Channels>           :<server> 254 <nick> <chans> :channels formed
  , "312"    -- <WHOIS - Server>     :<server> 312 <nick> <nick> <server> :<server location>
  , "317"    -- <WHOIS - Activity>   :<server> 317 <nick> <nick> <seconds_idle> <signon_time> :seconds idle, signon time
  , "318"    -- <WHOIS - End>        :<server> 318 <nick> <nick> :End of /WHOIS list.
  , "366"    -- <NAMES>              :<server> 366 <nick> <chan> :End of /NAMES list.
  , "376"    -- <MOTD>               :<server> 376 <nick> :End of /MOTD command.
  , "378"    -- <WHOIS - IP>         :<server> 378 <nick> <nick> :is connecting from <ip_and_something> <ip>
  ]


-- #### Msg Parsing ####

parseMsg :: String -> Msg
parseMsg m
  | msgType == "PRIVMSG" = UserMsg   sender (head args) rest  -- <User Msg>         :<from> PRIVMSG <to> :<msg>
  | msgType == "NICK"    = UserNick  sender rest              -- <User Nick Change> :<old_nick> NICK :<new_nick>
  | msgType == "JOIN"    = UserJoin  sender (head args)       -- <User Join>        :<nick> JOIN <chan>
  | msgType == "PART"    = UserPart  sender (head args)       -- <User Part>        :<nick> PART <chan>
  | msgType == "QUIT"    = UserQuit  sender rest              -- <User Quit>        :<nick> QUIT :<msg>
  | msgType == "KICK"    = Kick      sender (args !! 0) (args !! 1) rest                  -- <User Kicked>      :<kicker> KICK <chan> <nick> :<reason>
  | msgType == "MODE"    = UserMode  sender (head args) (unwords ((tail args) ++ [rest])) -- <User Mode Change> :<changer> MODE <changee> <mode> :[args]
  | msgType == "311"     = UserHost  (args !! 1) (args !! 3)  -- <User Host Query>  :<server> 311 <requester> <nick> <username> <hostname> * :<realname>
  | msgType == "331"     = ChanTopic (args !! 1) rest         -- <Channel Topic>    :<server> 331 <nick> <chan> :No topic is set.
  | msgType == "332"     = ChanTopic (args !! 1) rest         -- <Channel Topic>    :<server> 332 <nick> <chan> :<topic>
  | msgType == "353"     = ChanNicks (args !! 2) (words rest) -- <Chan Nick List>   :<server> 353 <nick> @ <chan> :<space separated nicks>
  | isPrefixOf "PING" m  = Ping $ drop (length "PING :") m    -- <Ping>             PING :<name_of_server_that_pinged>
  | msgType `elem` msgTypeServMsg = ServMsg sender rest       -- <Server Msg>       (contents varies)
  | msgType `elem` msgTypeIgnore  = Ignored                   -- <Ignored>          (contents varies)
  | otherwise = Unknown m                                     -- <Unknown>          (contents varies)
    where (sender,  _rest1) = break (== ' ') (drop 1 m)      -- The sender: the first word of the message
          (msgType, _rest2) = break (== ' ') (drop 1 _rest1) -- The message type: the second word of the message
          (_args,   _rest3) = break (== ':') (drop 1 _rest2) -- The arguments (as a single string): if ':'' exists, all the words up until that point, else all the remaining words
          args = words _args   -- The seperated arguments
          rest = drop 1 _rest3 -- The rest of the message: everything after the ':' (empty if ':' doesn't exist)


-- #### Msg Replying ####

replyMsg :: Server -> Msg -> IO IRC
replyMsg c@(Server i h) (UserMsg from to content) = do
  oPrint $ from ++ userMsgDestination ++ to ++ userMsgPrefix ++ content
  
  -- ignore all private messages
  when (to /= botNick i) $ do
    let urls = getURLsFromText content
    -- if there are URLs in the message, return info about them
    when (urls /= []) $ do
      titles <- getMetadataFromURLs urls
      let prettifiedTitles = intercalate ", " $ map (\x -> "[" ++ x ++ "]") titles
      sendMsg c to prettifiedTitles
    
    -- don't interpret our own messages so we don't loop on a command like
    -- "!echo !echo !echo".
    when (from /= botNick i) $ do
      reply <- doUserCommand content
      when (reply /= "") $ do
        replyMsg c (UserMsg (botNick i) to reply)
        sendMsg  c to reply
  return i

replyMsg (Server i h) (UserNick from to)      = oPrint (userActPrefix ++ from ++ " is now known as " ++ to) >> return i
replyMsg (Server i h) (UserJoin nick chan)    = oPrint (userActPrefix ++ nick ++ " has joined " ++ chan) >> return i
replyMsg (Server i h) (UserPart nick chan)    = oPrint (userActPrefix ++ nick ++ " has left "   ++ chan) >> return i
replyMsg (Server i h) (UserQuit nick msg)     = oPrint (userActPrefix ++ nick ++ " has quit (" ++ msg ++ ")") >> return i
replyMsg (Server i h) (Kick from chan to msg) = oPrint (userActPrefix ++ from ++ " kicked " ++ to ++ " from channel " ++ chan ++ " (" ++ msg ++ ")") >> return i
replyMsg (Server i h) (UserMode from to mode) = oPrint (userActPrefix ++ from ++ " sets " ++ to ++ " to mode " ++ mode) >> return i
replyMsg (Server i h) (UserHost nick host)    = oPrint (userActPrefix ++ nick ++ " has host " ++ host) >> return (setHost i nick host)
replyMsg (Server i h) (ChanTopic chan topic)  = oPrint (chanMsgPrefix ++ "Topic for " ++ chan ++ ": " ++ topic) >> return i
replyMsg (Server i h) (ChanNicks chan nicks)  = oPrint (chanMsgPrefix ++ "Users connected to " ++ chan ++ ": " ++ intercalate ", " nicks) >> return i
replyMsg (Server i h) (ServMsg from msg)      = oPrint (from ++ servMsgPrefix ++ msg) >> return i
replyMsg (Server i h) (Ping from)             = write h "PONG" (":" ++ from) >> return i
replyMsg (Server i h) Ignored                 = return i
replyMsg (Server i h) (Unknown content)       = ePrint ("Error in replyMsg -- Unknown Message: " ++ content) >> return i

setHost :: IRC -> String -> String -> IRC
setHost irc nick host = if botNick irc == nick
  then irc { botHost = host }
  else irc
