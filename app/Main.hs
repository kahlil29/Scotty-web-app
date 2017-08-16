{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Lens
import           Control.Monad.IO.Class
import           Data.Aeson              (FromJSON, ToJSON, Value, decode,
                                          encode)
import           Data.Map                as Map
import           Data.Maybe
import           Data.String.Conv
import           Data.Text
import qualified Data.Text               as T
import           Data.Time
import           Data.Time.Locale.Compat
import           GHC.Generics
import           Lib
import           Network.API.TheMovieDB
import           Network.Wreq            as Wreq
import           Text.Printf
import           Web.Scotty              as Scotty

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

-- main :: IO ()
-- main =
--   scotty 3000 $
--    Scotty.get "/" $ do
--     r <- liftIO $ Wreq.get "https://www.metaweather.com/api/location/search/?query=san"
--     let weatherInfoArray = decode (r ^. responseBody) :: Maybe [WeatherInfo]
--
--     raw $ toS (show weatherInfoArray)
--     -- raw (r ^. responseBody)
--     -- Data.String.Conv

main :: IO ()
main = do
  args <- getArgs
  let key = maybe T.empty T.pack (listToMaybe args)

  result <- runTheMovieDB key $
    case args of
      [_, "search", query]  -> searchAndListMovies (T.pack query)


searchAndListMovies :: Text -> TheMovieDB ()
searchAndListMovies query = do
  movies <- searchMovies query
  liftIO $ mapM_ printMovieHeader movies


printMovieHeader :: Movie -> IO ()
printMovieHeader m =
 printf "%8d: %s (%s)\n" (movieID m) (T.unpack $ movieTitle m) year
 where year = case movieReleaseDate m of
                Just d  -> formatTime defaultTimeLocale "%Y" d
                Nothing -> "----"
