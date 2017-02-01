{-# LANGUAGE DeriveGeneric #-}

module Data.Option where

import Data.Aeson as A
import GHC.Generics

data Raw  = Raw { raw :: Double
                , fmt :: Maybe String
                , longFmt :: Maybe String
                }
          deriving (Show, Generic)

instance A.FromJSON Raw

data Option = Option { percentChange :: Raw
                     , openInterest :: Raw
                     , change :: Raw
                     , strike :: Raw
                     , inTheMoney :: Bool
                     , impliedVolatility :: Raw
                     , volume :: Raw
                     , contractSymbol :: String
                     , ask :: Raw
                     , lastTradeDate :: Raw
                     , expiration :: Raw
                     , currency :: String
                     , contractSize :: String
                     , bid :: Raw
                     , lastPrice :: Raw
                     }
          deriving (Show, Generic)

instance A.FromJSON Option
