module ReadWeather
    (getWeather,apikey,host,packStr)
     where

import qualified Network.HTTP as H
import qualified Data.ByteString.Lazy.Internal as B


-- | API Key to construct the weather url check http://api.openweathermap.org for more info
apikey :: String
apikey = "8a45355d32fce331dcdaba2dcfaa8719"

-- | Host website url
host :: String
host = "http://api.openweathermap.org/data/2.5/weather?q="      -- Actual url for weather request -> "host ++ cityname ++ &appid= ++ apikey"

packStr :: [Char] -> B.ByteString
packStr = B.packChars

-- | Read the weather information as Json string
getWeather :: String -> IO String
getWeather city = do
    let wurl = host ++ city ++ "&appid=" ++ apikey
    rsp  <- H.simpleHTTP (H.getRequest wurl)
    H.getResponseBody rsp