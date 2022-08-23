{-# LANGUAGE TemplateHaskell #-}

-- 
-- Module for atomic printing/network message sending. The 'printer' function
-- needs to be invoked ('forkIO printer', probably) to actually print/send
-- the messages.
-- 
-- Any '{e,o,n}Print[BS]' function, if called before 'printer' is started,
-- should just add the message to a queue which will be safely consumed when
-- 'printer' is eventually started.
-- 

module Print (oPrint, ePrint, nPrint, nPrintBS, printer, Print(..)) where

import Control.Concurrent
import Control.Monad
import Data.Global
import Data.Monoid
import System.IO
import System.Console.ANSI
import qualified Data.ByteString      as BSW8
import qualified Data.ByteString.UTF8 as BSU8

import Config


-- #### Defines ####

data Print = Stdout String | Stderr String |
             Network Handle String | NetworkBS Handle BSW8.ByteString

declareChan "ch"  [t| Print |]


-- #### Main ####

oPrint :: String -> IO ()
oPrint xs = writeChan ch (Stdout xs)

ePrint :: String -> IO ()
ePrint xs = writeChan ch (Stderr xs)

nPrint :: Handle -> String -> IO ()
nPrint h xs = writeChan ch (Network h xs)

nPrintBS :: Handle -> BSW8.ByteString -> IO ()
nPrintBS h xs = writeChan ch (NetworkBS h xs)

printer :: IO ()
printer = forever $ do
    setupColors
    next <- readChan ch
    case next of
      (Stdout xs) -> hPutStrLn stdout xs
      (Stderr xs) -> hPutStrLn stderr xs
      (Network   h xs) -> do hPutStr h xs
                             when debug $ hPutStr stderr (debugOut ++ xs)
      (NetworkBS h xs) -> do BSW8.hPutStr h xs
                             when debug $ BSW8.hPutStr stderr (BSU8.fromString debugOut <> xs)


-- #### Util ####

setupColors :: IO ()
setupColors = do hSetSGR stdout [SetColor Foreground Vivid White]
                 hSetSGR stderr [SetColor Foreground Vivid Black]
