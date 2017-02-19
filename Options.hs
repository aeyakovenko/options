{-# LANGUAGE OverloadedStrings #-}
module Options where
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List(foldl', partition, groupBy)
import Data.Time.Calendar(Day, fromGregorian, addDays)
import Data.Maybe(fromMaybe)
import Data.Function(on)
import Data.Array(listArray, Array, bounds, (!))

-- symbol,exchange,date,adjusted stock close price,option symbol,expiration,strike,call/put,style,ask,bid,volume,open interest,unadjusted stock price
-- SPY,NYSEArca,01/07/05,118.44,SYHXD,12/17/05,160,P,A,0,0,0,0,118.44
data Type = Call | Put
          deriving Enum

data Style = American
type StrikePrice = Int
data Quote = Quote { symbol :: C.ByteString
                   , exchange :: C.ByteString
                   , date :: Day
                   , adjustedPrice :: Double
                   , optionSymbol :: C.ByteString
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

sellPuts :: Options -> Account -> Account
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

buyPuts :: Options -> Account -> Account
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
          (hi, low) = spread $ alg ac
          hp = (ceiling $ hi * p/5) * 5
          lp = (floor $ low * p/5) * 5
          e = fromIntegral $ expired $ alg ac
          d = addDays e (date $ os ! lb)

parse :: C.ByteString -> [Quote]
parse ln | C.head ln == '#' = []
parse ln = [Quote sm ex (toDay da) (readC ap) os (toDay exp) (readC st) (toType cp) (toStyle sy) (readC as) (readC bi) (readC vo) (readC oi) (readC us)]
    where [sm,ex,da,ap,os,exp,st,cp,sy,as,bi,vo,oi,us] = C.split ',' ln
          toType "C" = Call 
          toType "P" = Put 
          toStyle :: C.ByteString -> Style
          toStyle "A" = American

readC s = read $ C.unpack s

toDay :: C.ByteString -> Day
toDay str = fromGregorian (readC yy + 2000) (readC mm) (readC dd) 
    where [mm,dd,yy] = C.split '/' str

main :: IO ()
main = do
    let grup = groupBy ((==) `on` expiration)
    quotes <- grup <$> concatMap parse <$> tail <$> C.lines <$> C.readFile "spy_options.1.7.2005.to.12.28.2009.r.csv"
    let alg = Alg (0.85, 0.8) 10000 0.1 0.8
    let ac = Account  10000 [] alg
    print $ foldl' trade ac  quotes 
