{-# LANGUAGE DeriveGeneric #-}
module Data.Quote where

import Data.Aeson as A
import GHC.Generics

data Quote = Quote { quoteType :: String
                   , quoteSourceName :: String
                   , currency :: String
                   , longName :: String
                   , regularMarketPrice :: Double
                   , regularMarketTime :: Integer
                   , regularMarketChange :: Double
                   , regularMarketOpen :: Double
                   , regularMarketDayHigh :: Double
                   , regularMarketDayLow :: Double
                   , regularMarketVolume :: Double
                   , shortName :: String
                   , sharesOutstanding :: Integer
                   , fiftyDayAverage :: Double
                   , fiftyDayAverageChange :: Double
                   , fiftyDayAverageChangePercent :: Double
                   , twoHundredDayAverage :: Double
                   , twoHundredDayAverageChange :: Double
                   , twoHundredDayAverageChangePercent :: Double
                   , marketCap :: Double
                   , sourceInterval :: Integer
                   , exchangeTimezoneName :: String
                   , exchangeTimezoneShortName :: String
                   , gmtOffSetMilliseconds :: Integer
                   , postMarketChangePercent :: Double
                   , postMarketTime :: Integer
                   , postMarketPrice :: Double
                   , postMarketChange :: Double
                   , regularMarketChangePercent :: Double
                   , regularMarketPreviousClose :: Double
                   , bid :: Double
                   , ask :: Double
                   , bidSize :: Double
                   , askSize :: Double
                   , messageBoardId :: String
                   , fullExchangeName :: String
                   , averageDailyVolume3Month :: Integer
                   , averageDailyVolume10Day :: Integer
                   , fiftyTwoWeekLowChange :: Double
                   , fiftyTwoWeekLowChangePercent :: Double
                   , fiftyTwoWeekHighChange :: Double
                   , fiftyTwoWeekHighChangePercent :: Double
                   , fiftyTwoWeekLow :: Double
                   , fiftyTwoWeekHigh :: Double
                   , exchange :: String
                   , market :: String
                   , marketState :: String
                   , symbol :: String
                   }
           deriving (Show, Generic)


instance A.FromJSON Quote
