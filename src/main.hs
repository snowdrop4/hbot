module Main where

import Prelude hiding (catch, log)
import Control.Monad hiding (join)
import Control.Concurrent
import Network
import System.IO
import System.Console.ANSI

import Config
import Commands.Admin
import IRC
import IRC.Messages
import IRC.Network
import Print


-- #### Defines ####

data Input = NetIn String | StdIn String
  deriving (Show)


-- #### Main ####

main :: IO ()
main = do
  irc <- getConfig -- read configuration from config file
  handle <- connectTo (botServ irc) (botPort irc) -- open handle to server
  
  setTitle $ botNick irc ++ " - " ++ botServ irc -- set the terminal title
  
  -- send nick/username/password/channel details to the server
  hSetBuffering handle NoBuffering
  hSetEncoding  handle utf8
  setPass handle (botPass irc)
  setName handle (botUser irc) (botName irc)
  setNick handle (botNick irc)
  getHost handle (botNick irc)
  mapM_ (join handle) (botChan irc)
  
  -- set up the data channel and fork two threads for network and stdin input,
  -- and one thread to handle atomic printing
  chan <- newChan :: IO (Chan Input)
  forkIO $ listenNetwork (Server irc handle) chan
  forkIO $ listenStdin chan
  forkIO $ printer
  reply (Server irc handle) chan

listenNetwork :: Server -> Chan Input -> IO ()
listenNetwork c@(Server i h) chan = forever $ do
  ni <- fmap init $ hGetLine h -- remove shitty \r character
  writeChan chan (NetIn ni)
  when debug $ ePrint $ debugIn ++ ni -- optionally print debug message

listenStdin :: Chan Input -> IO ()
listenStdin chan = forever $ getLine >>= writeChan chan . StdIn

reply :: Server -> Chan Input -> IO ()
reply c@(Server i h) chan = do
  msg <- readChan chan
  case msg of
    (NetIn a) -> do newI <- replyMsg c (parseMsg a)
                    reply (Server newI h) chan
    (StdIn a) -> do newI <- doAdminCommand c a
                    reply (Server newI h) chan
