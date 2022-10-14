import qualified Network.HTTP as H
import qualified Data.Text as T
import           Control.Monad.Trans.State ( StateT(runStateT))
import           Data.Aeson (decode)
import           ConfigData ( printItems, readInt, readKeys, readSections )
import           ReadWeather ( apikey, host, packStr )
import           JSONtoData ( printWeather, WeatherData )
import           Data.Text ()
import           Mail_imap

main :: IO()
main = do
    putStrLn  "<---------- Weather Appliction ---------->"
    let config = "config.ini"
    (sec, c) <- runStateT readSections config
    putStrLn $ printItems sec
    putStrLn "Enter the respective number to select the Country: "
    cuntryIndex <- readInt
    let k = readKeys (sec !! (cuntryIndex-1))
    (key, _) <- runStateT k c
    Prelude.putStrLn $ printItems key
    putStrLn "Enter respective number to select the city: "
    cityIndex <- readInt
    let city = T.unpack (key !! (cityIndex-1))
    let wurl = host ++ city ++ "&appid=" ++ apikey
    putStrLn $ "Weather report for " ++ city ++ " below:\n"
    rsp  <- H.simpleHTTP (H.getRequest wurl)
    content  <- H.getResponseBody rsp
    let weather = decode $ packStr content :: Maybe WeatherData
    case weather of
        Nothing -> putStrLn "Invaild Data"
        Just x -> smtpTest "Weather Data" (show x) >> imapTest >> return ()
