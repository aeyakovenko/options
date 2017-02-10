module YFO where
import qualified Data.ByteString.Lazy as L
import Data.List(foldl')
import Data.Time.Calendar(Day)

-- symbol,exchange,date,adjusted stock close price,option symbol,expiration,strike,call/put,style,ask,bid,volume,open interest,unadjusted stock price
data OptionType = Call | Put
data Quote = Quote { symbol :: String
                   , exchange :: String
                   , date :: Day
                   , adjustedPrice :: Double
                   , optionSymbol :: String
                   , expiration :: Day
                   , strike :: Double
                   , callOrPut :: OptionType
                   , style :: String
                   , ask :: Double
                   , bid :: Double
                   , volume :: Double
                   , openInterest :: Double
                   , unadjustedStockPrice :: Double
                   }
parse ln = 
    let [sy,ex,da,as,os,exp,st,cp,st,as,bi,vo,oi,us] = splitBy "," ln
    in Quote sy ex (fromGregorian da) (read as) os (fromGregorian exp) (read st) (toOptionType cp) st (read as) (read bi) (read vo) (read oi) (read us)

main :: IO ()
main = do
    lines <- tail <$> L.lines <$> L.readFile "spy_options.1.7.2005.to.12.28.2009.csv"
    foldl' trade account lines 
