{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module JSONtoData where

import Data.Aeson (FromJSON (parseJSON), decode)
import Data.Aeson.Types ( (.:), withObject )
import Data.Aeson.Key (fromString)
import GHC.Generics ( Generic )
import Numeric (showFFloat)

data WeatherData where
  APIError :: String -> WeatherData
  Weather :: {temperature :: Double,
              feels_like :: Double,
              min_temp :: Double,
              max_temp :: Double,
              humidity :: Int}
             -> WeatherData
  deriving Generic


instance FromJSON WeatherData where
  parseJSON = withObject "WeatherData" $ \obj -> do
    (code :: Int) <- obj .: fromString "cod"
    if code == 401
      then fmap APIError (obj .: fromString "message")
      else do
        mainO    <- obj .: fromString "main"
        --weatherO <- obj .: fromString "weather"
        --discription <- weatherO .: fromString "discription"
        temperature <- mainO    .: fromString "temp"
        feels_like  <- mainO    .: fromString "feels_like"
        min_temp    <- mainO    .: fromString "temp_min"
        max_temp    <- mainO    .: fromString "temp_max"
        humidity    <- mainO    .: fromString "humidity"
        return Weather {..}

instance Show WeatherData where
 show :: WeatherData -> String
 show (Weather t f min_temp max_temp h) =  "Current Temperature:    "   ++  format (t - 273.15) ++ " degC" ++
                                           "\nTemperature Feels Like: " ++ format (f - 273.15) ++ " degC" ++
                                           "\nMaximum Temperature:    " ++  format (max_temp - 273.15) ++ " degC" ++
                                           "\nMinimum Temperature:    " ++ format (min_temp - 273.15) ++ " degC" ++
                                           "\nHumidity Level:         " ++ show h ++ " g.kg^-1"

format :: Double -> String
format x = showFFloat (Just 2) x ""