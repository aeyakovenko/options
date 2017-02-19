module Options where
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as C
import Data.List(foldl', partition, groupBy)
import Data.List.Split(splitOn)
import Data.Time.Calendar(Day, fromGregorian)
import Data.Maybe(fromMaybe)
import Data.Function(on)
import Data.Array(listArray, Array, bounds, (!))

-- symbol,exchange,date,adjusted stock close price,option symbol,expiration,strike,call/put,style,ask,bid,volume,open interest,unadjusted stock price
-- SPY,NYSEArca,01/07/05,118.44,SYHXD,12/17/05,160,P,A,0,0,0,0,118.44
data Type = Call | Put
          deriving Enum

data Style = American
type StrikePrice = Int
data Quote = Quote { symbol :: String
                   , exchange :: String
                   , date :: Day
                   , adjustedPrice :: Double
                   , optionSymbol :: String
                   , expiration :: Day
                   , strike :: StrikePrice
                   , callOrPut :: Type
                   , style :: Style
                   , ask :: Double
                   , bid :: Double
                   , volume :: Int
                   , openInterest :: Int
                   , unadjustedStockPrice :: Double
                   }

data Alg = Alg { spread :: (Double, Double)
               , expired :: Int
               , investRatio :: Double
               , sellThreshold :: Double
               }

type Options = Array (Day,StrikePrice) Quote
data Account = Account { cash :: Double
                       , puts :: [(Quote, Quote, Int)]
                       , alg :: Alg
                       }
maxPrice = 2**32

trade ::  Account -> [Quote] -> Account
trade ac qs = buyPuts os $ sellPuts os ac
    where os = listArray (l,h) ls
          ls = map (\ q -> (toix q, q)) qs
          l = fst $ head ls
          h = fst $ last ls

sellValue os (h,l,v)= fromMaybe 0 $ do 
    hp <- bid <$> lookupO os (toix h)
    lp <- ask <$> lookupO os (toix l)
    return $ v * (hp - lp)

toix q = (expiration q, strike q)

maxValue (h,l,v) = (strike h) - (strike l) * v

sellPuts os ac = ac { cash = nc, puts = keep }
    where needsClose (q,_,_) | (expiration q) - d < 7 = True
          needsClose pt | sellValue os pt / maxValue pt > (sellThreshold $ alg $ ac) = True
          needsClose pt | sellValue os pt == 0 = True
          needsClose _ = False
          (close,keep) = partition needsClose (puts ac)
          (lb,hb) = bounds os
          d = date $ os ! lb
          nc = cash ac + (sum $ map (sellValue os) close)

lookupO os ix = lookup' ix
    where (lo,hi) = bounds os
          lookup' ix | ix < lo || ix > hi = Nothing
          lookup' ix | otherwise =  Just $ os ! ix

buyPuts os ac = fromMaybe ac $ do
    hq <- lookup (d,hp)
    lq <- lookup (d,lp)
    let bp = (ask hq - bid lq)
        s = investRatio ac * c
        v = floor $ s / bp
        nc = c - v * bp
        np = (hq, lq, v) : puts ac
    return $ ac { cash = nc, puts = np }
    where c = cash ac
          p = unadjustedStockPrice (os ! lb)
          (lb,hb) = bounds os
          (hi, low) = spread ac
          hp = (ceiling $ hi * p/5) * 5
          lp = (floor $ low * p/5) * 5
          d = expired ac + (date $ os ! lb)

parse :: String -> [Quote]
parse ('#':_) = []
parse ln = [Quote sm ex (toDay da) (read ap) os (toDay exp) (read st) (toType cp) (toStyle sy) (read as) (read bi) (read vo) (read oi) (read us)]
    where [sm,ex,da,ap,os,exp,st,cp,sy,as,bi,vo,oi,us] = splitOn "," ln
          toType "C" = Call 
          toType "P" = Put 
          toStyle "A" = American

toDay :: String -> Day
toDay str = fromGregorian (read yy + 2000) (read mm) (read dd) 
    where [mm,dd,yy] = splitOn "/" str

main :: IO ()
main = do
    let grup = groupBy ((==) `on` expiration)
    quotes <- grup <$> concatMap parse <$> tail <$> C.lines <$> L.readFile "spy_options.1.7.2005.to.12.28.2009.r.csv"
    let alg = Alg (0.85, 0.8) 10000 0.1 0.8
    let ac = Account  10000 [] alg
    foldl' trade ac  quotes 
