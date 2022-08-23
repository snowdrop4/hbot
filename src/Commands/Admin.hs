module Commands.Admin (doAdminCommand) where

import Data.List
import Commands
import Config
import Print
import IRC
import qualified IRC.Messages as M
import qualified IRC.Network  as N


-- #### Defines ####

type AdminFunc = Server -> [String] -> IO IRC


-- #### Util ####

wrongNumArgs' c@(Server i h) = oPrint wrongNumArgs >> return i


-- #### Main ####

doAdminCommand :: Server -> String -> IO IRC
doAdminCommand c@(Server i h) xs = case stripWhitespace xs of
  [] -> return i
  _  -> runAdminCommand c xs

runAdminCommand :: Server -> String -> IO IRC
runAdminCommand c@(Server i h) x =
  let func = lookupCommand adminCommands $ head (words x)
      args = tail (words x) in
  case func of
    (Just f) -> f c args
    Nothing  -> oPrint ("Error: command '" ++ x ++ "' not found") >> return i


-- #### Admin Commands ####

-- promote the help function that's defined for user commands to one that has the type of an admin command
adminHelp :: AdminFunc
adminHelp c@(Server i h) x = oPrint (help adminCommands x) >> return i

sendMsg :: AdminFunc
sendMsg c@(Server i h) (to:x:xs) = do
  let msg = unwords (x:xs)
  N.sendMsg c to msg
  M.replyMsg c (M.UserMsg to (botNick i) msg)
  oPrint "Success: message sent"
  return i
sendMsg c _ = wrongNumArgs' c

leave :: AdminFunc
leave c@(Server i h) (x:[]) = let newChanList = filter (/= x) (botChan i) in
  case x `elem` botChan i of
    True  -> do N.leave h x
                oPrint $ "Success: left channel " ++ x
                return i {botChan = newChanList}
    False -> oPrint ("Error: not in channel " ++ x) >> return i
leave c _ = wrongNumArgs' c

join :: AdminFunc
join c@(Server i h) (x:[]) = case x `elem` botChan i of
  True  -> oPrint ("Error: already in channel " ++ x) >> return i
  False -> do N.join h x
              oPrint $ "Success: joined channel " ++ x
              return i
join c _ = wrongNumArgs' c

channels :: AdminFunc
channels c@(Server i h) [] = oPrint ((botNick i)
  ++ " is in: "
  ++ (intercalate ", " (botChan i)))
    >> return i
channels c _ = wrongNumArgs' c

nick :: AdminFunc
nick c@(Server i h) (x:[]) = do
  N.setNick h x
  oPrint $ "Success: set nick to " ++ x
  return i {botNick = x}
nick c _ = wrongNumArgs' c

kick :: AdminFunc
kick c@(Server i h) (chan:nick:msg) = do
  let msg' = if null msg then (botNick i) else unwords msg
  N.kick h chan nick msg'
  return i
kick c _ = wrongNumArgs' c

ban :: AdminFunc
ban c@(Server i h) s@(chan:nick:mask:msg) = do
  mode c (chan:"+b":mask:[])
  kick c (chan:nick:msg)
ban c _ = wrongNumArgs' c

mode :: AdminFunc
mode c@(Server i h) (set:flags:args) = N.mode h set flags (unwords args) >> return i
mode c _ = wrongNumArgs' c

adminCommands :: [(CommandName, AdminFunc, CommandHelp)]
adminCommands = 
  [ ("help", adminHelp,
     "[command-name] -- if no argument given, prints list of admin commands, else prints help for specific admin command")
  , ("msg", sendMsg,
     "<channel/user> <message>[...] -- sends a message to the specified destination")
  , ("leave", leave,
     "<channel> -- leaves the specified channel")
  , ("join", join,
     "<channel> -- joins the specified channel")
  , ("channels", channels,
    "-- lists currently joined channels")
  , ("nick", nick,
    "<new-nickname> -- changes the bot nickname")
  , ("kick", kick,
    "<channel> <user> [<message>[...]] -- kicks the user from the channel, if no message is given it defaults to <bot-username>")
  , ("ban", ban,
    "<channel> <user> <ban-mask> [<message>[...]] -- kickbans the user from the channel, if no message is given it defaults to <bot-username>")
  , ("mode", mode,
    "<channel/user> <flags> <args> -- sets the mode of the thing") ]
