{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson             (FromJSON, ToJSON, Value, decode,
                                         encode)
import           Data.Map               as Map
import           Data.String.Conv
import           GHC.Generics
import           Lib
import           Network.Wreq           as Wreq
import           Web.Scotty             as Scotty

-- type Resp = Response (Map String Value)

data WeatherInfo = WeatherInfo {
               title         :: String,
               location_type :: String,
               woeid         :: Int,
               latt_long     :: String
} deriving (Show, Generic)
instance ToJSON WeatherInfo
instance FromJSON WeatherInfo

-- main :: IO ()
-- main =
--   scotty 3000 $
--    Scotty.get "/" $ do
--     -- html "Hello World!"
--     response <- Wreq.get"https://www.metaweather.com/api/location/search/?query=New%20York"
--     raw (response ^. responseBody)
-- encode response and then toHtml

main :: IO ()
main =
  scotty 3000 $
   Scotty.get "/" $ do
    r <- liftIO $ Wreq.get "https://www.metaweather.com/api/location/search/?query=New%20York"
    let weatherInfoArray = decode (r ^. responseBody) :: Maybe [WeatherInfo]
    raw $ toS (show weatherInfoArray)
    -- raw (r ^. responseBody)
    -- Data.String.Conv
