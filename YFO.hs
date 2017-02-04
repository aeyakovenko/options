module YFO where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as L
import qualified Data.YFO as Y
import qualified Data.Option as Y
import qualified Data.OptionLists as Y
import qualified Data.Time.Clock as C
import qualified Data.Time.Clock.POSIX as P
import qualified Data.Time.Calendar as D

import Data.Char(toUpper)

-- URL = https://query2.finance.yahoo.com/v7/finance/options/SPY?date=1516320000

query :: String -> D.Day -> String 
query s e = concat ["https://query2.finance.yahoo.com/v7/finance/options/", u, "?date=", t]
    where t = dayToPosixTime e
          u = map toUpper s

dayToPosixTime :: D.Day -> Integer
dayToPosixTime = t
    where t = P.utcTimeToPOSIXSeconds (C.UTCTime e (C.secondsToDiffTime (13 + 60)))
          u = map toUpper s

today :: IO Integer
today = map (dayToPosixTime . C.utctDay) t
    where t = getCurrentTime

parse :: L.ByteString -> Y.OptionChain (Y.Result [Y.OptionsData])
parse = from . A.eitherDecode
    where from (Left err) = error err
          from (Right v) = v

data OptionPrice = Call { date :: Integer
                        , expiration :: Integer
                        , strike :: Double
                        , price :: Double
                        }
                 | Put { date :: Integer
                       , expiration :: Integer
                       , strike :: Double
                       , price :: Double
                       }
                deriving Show

toOptionPrices :: Y.OptionChain (Y.Result [Y.OptionsData]) -> [OptionPrice]

fromOptionsData :: Y.OptionsData -> [OptionPrice]

toOptionPrices (Y.OptionChain (Y.Result ls _)) = concatMap fromOptionsData ls

fromOptionsData (Y.OptionsData {Y.options = ls}) = concatMap fromOptionLists ls

fromOptionLists :: Y.OptionLists -> [OptionPrice]
fromOptionLists (Y.OptionLists { Y.calls = cs, Y.puts = ps }) = 
        map (fromOption Call) cs
     ++ map (fromOption Put) ps

fromOption :: (Integer -> Integer -> Double -> Double -> OptionPrice) -> Y.Option -> OptionPrice
fromOption v o = v d e s p
    where d = Y.lastTradeDate o
          e = Y.expiration $ o
          s = Y.strike o 
          p = Y.lastPrice o 

line :: Fractional a => (a, a) -> (a, a) -> a -> a
line (x1,y1) (x2,y2) x3 = s * x3 + c
    where s = (y2 - y1)/(x2 - x1)
          c = y1 - s * x1

main :: IO ()
main = do
    f <- L.readFile "data/test.json"
    print $ toOptionPrices $ parse f
