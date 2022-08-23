module IRC where

import System.IO

import Network


-- #### Main ####

data IRC = IRC
  { botServ :: String
  , botPort :: PortID
  , botChan :: [String]
  , botNick :: String
  , botUser :: String
  , botName :: String
  , botPass :: String
  , botHost :: String }  -- needs to be set on connection

data Server = Server
  { irc     :: IRC
  , botHand :: Handle }
