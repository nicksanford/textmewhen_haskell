{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

import GHC.Generics
import Data.Maybe
import Control.Lens
import Network.Wreq
import System.Environment
import Data.Aeson
import Data.Aeson.Lens (nth, key)

data UberPriceEstimateParams = UberPriceEstimateParams  { startLatitude :: String
                                                        , startLongitude :: String
                                                        , endLatitude :: String
                                                        , endLongitude :: String
                                                        } deriving (Show)
parseToUberXData :: Maybe a -> Maybe b
parseToUberXData (Just x)   = Just UberXData   { currency_code     = x . key "currency_code"
                                              , display_name      = x . key "diplay_name"
                                              , distance          = x . key "distance"
                                              , duration          = x . key "duration"
                                              , estimate          = x . key "estimate"
                                              , high_estimate     = x . key "high_estimate"
                                              , low_estimate      = x . key "low_estimate"
                                              , product_id        = x . key "product_id"
                                              , surge_multiplier  = x . key "surge_multiplier"
                                              }
parseToUberXData (Just _)   = Nothing
parseToUberXData Nothing    = Nothing

data UberXData = UberXData  { currency_code     :: String
                            , display_name      :: String
                            , distance          :: Double
                            , duration          :: Double
                            , estimate          :: String
                            , high_estimate     :: Double
                            , low_estimate      :: Double
                            , product_id        :: String
                            , surge_multiplier  :: Double
                            } deriving (Show, Eq, Ord, Generic)


instance FromJSON UberXData
instance ToJSON UberXData

uberPriceEstimatesUrl = "https://api.uber.com/v1/estimates/price"
priceEstimateQuery a b c d = defaults & param "server_token"    .~ ["-xkRmo_rM8qCckl-kJQf5Vg8HbkRYN4VLcgYc-CS"]
                                      & param "start_latitude"  .~ [a]
                                      & param "start_longitude" .~ [b]
                                      & param "end_latitude"    .~ [c]
                                      & param "end_longitude"   .~ [d]

main :: IO ()
main = do
  response <- getWith (priceEstimateQuery "30.241518" "-97.735293" "30.366752" "-97.680876") uberPriceEstimatesUrl
  let uberXPrices = response ^. responseBody ^? key "prices" . nth 0
  print $ parseToUberXData uberXPrices
