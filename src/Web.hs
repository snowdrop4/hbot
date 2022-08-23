module Web (getMetadataFromURLs, getURLsFromText) where

import Prelude hiding (catch)
import Data.Char
import Data.List
import Data.Maybe
import Control.Exception
import Network.HTTP.Conduit
import Network.URI
import Network.TLS
import Text.HTML.TagSoup
import qualified Data.ByteString.Lazy.UTF8 as BLU8

import Print


-- #### Defines ####

data WebPage = WebPage
  { url  :: String
  , html :: [Tag String] }


-- #### Main ####

-- Returns any URLs (starting with http(s)://) in the text, if present
getURLsFromText :: String -> [String]
getURLsFromText str
 = filter (\x -> isURI x && ("http://" `isPrefixOf` x || "https://" `isPrefixOf` x))
          (words str)

-- Returns the title(s) of the the URL(s).
getMetadataFromURLs :: [String] -> IO [String]
getMetadataFromURLs urls = do
  pages <- mapM getWebPageFromURL urls
  return $ filter (/= "") $ map getTitleFromWebPage (catMaybes pages)


-- #### Network ####

-- Returns the WebPage of an URL.
getWebPageFromURL :: String -> IO (Maybe WebPage)
getWebPageFromURL url = do
  html <- getHTMLFromURL url 2
  case html of
    Nothing -> return Nothing
    _       -> return $ Just $ WebPage url (parseTags (fromJust html))

-- Tries to fetch the HTML from the URL. If an exception occurs it will retry
-- 'retry' times.
getHTMLFromURL :: String -> Int -> IO (Maybe String)
getHTMLFromURL url retry
  | retry == 0 = return Nothing
  | otherwise  = fmap (Just . BLU8.toString) (simpleHttp url) `catches`
                 [ Handler (\e -> ePrint (printPrefix ++ show (e :: IOException))   >> tryAgain)
                 , Handler (\e -> ePrint (printPrefix ++ show (e :: HttpException)) >> tryAgain)
                 , Handler (\e -> ePrint (printPrefix ++ show (e :: TLSError))      >> tryAgain) ]
  where tryAgain = getHTMLFromURL url (retry - 1)
        printPrefix = "Error in getHTMLFromURL -- Caught Exception: "


-- #### Title ####

-- Returns the title of a WebPage, if present.
getTitleFromWebPage :: WebPage -> String
getTitleFromWebPage (WebPage url html) =
  unwords $ words title -- remove excess spaces
    where first = drop 1 $ dropWhile (not . isTitleTag) html
          title = fromMaybe "" $ listToMaybe (take 1 first) >>= maybeTagText

isTitleTag (TagOpen x _) = (map toLower x) == "title"
isTitleTag _             = False
