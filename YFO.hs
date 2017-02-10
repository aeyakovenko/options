module YFO where
import qualified Data.ByteString.Lazy as L
import Data.List(foldl')
import Data.List.Split(splitOn)
import Data.Time.Calendar(Day, fromGregorian)

-- symbol,exchange,date,adjusted stock close price,option symbol,expiration,strike,call/put,style,ask,bid,volume,open interest,unadjusted stock price
-- SPY,NYSEArca,01/07/05,118.44,SYHXD,12/17/05,160,P,A,0,0,0,0,118.44
data Type = Call | Put
data Style = American
data Quote = Quote { symbol :: String
                   , exchange :: String
                   , date :: Day
                   , adjustedPrice :: Double
                   , optionSymbol :: String
                   , expiration :: Day
                   , strike :: Double
                   , callOrPut :: Type
                   , style :: Style
                   , ask :: Double
                   , bid :: Double
                   , volume :: Double
                   , openInterest :: Double
                   , unadjustedStockPrice :: Double
                   }

parse ln = Quote sy ex (toDay da) (read as) os (toDay exp) (read st) (toType cp) (toStyle sy) (read as) (read bi) (read vo) (read oi) (read us)
    where [sy,ex,da,as,os,exp,st,cp,sy,as,bi,vo,oi,us] = splitOn "," ln
          toType "C" = Call 
          toType "P" = Put 
          toStyle "A" = American

toDay :: String -> Day
toDay str = fromGregorian (read yy + 2000) (read mm) (read dd) 
    where [mm,dd,yy] = splitOn "/" str

data Account = Account { cash :: Double 
                       , stock :: [(Quote, Int)]
                       , puts :: [(Quote, Int)]
                       }

buyStock :: Account -> Quote -> Int -> Account
buyStock a q v = a { cash = nc, stock = ns }
    where s = m * (adjutedPrice q)
          m = (floor $ (cash a) / (adjutedPrice q)) `min` v
          nc = (cash a) - s
          ns = (q,m) : (stock a)

buyPut :: Account -> Quote -> Int -> Account
buyPut a q v = a { cash = nc, puts = ns }
    where s = m * (adjutedPrice q)
          m = (floor $ (cash a) / (adjutedPrice q)) `min` v
          nc = (cash a) - s
          ns = (q,m) : (stock a)

main :: IO ()
main = do
    quotes <- map parse <$> tail <$> L.lines <$> L.readFile "spy_options.1.7.2005.to.12.28.2009.r.csv"
    foldl' trade account quotes 
