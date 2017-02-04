{-# LANGUAGE DeriveGeneric #-}

module Data.Option where

import Data.Aeson as A
import GHC.Generics

data Option = Option { percentChange :: Double
                     , openInterest :: Integer
                     , change :: Double
                     , strike :: Double
                     , inTheMoney :: Bool
                     , impliedVolatility :: Double
                     , volume :: Integer
                     , contractSymbol :: String
                     , ask :: Double
                     , lastTradeDate :: Integer
                     , expiration :: Integer
                     , currency :: String
                     , contractSize :: String
                     , bid :: Double
                     , lastPrice :: Double
                     }
          deriving (Show, Generic)

instance A.FromJSON Option
