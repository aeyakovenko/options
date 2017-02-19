{-# LANGUAGE OverloadedStrings #-}
module Options where
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List(foldl', partition, groupBy)
import Data.Time.Calendar(Day, fromGregorian, addDays, diffDays)
import Data.Maybe(fromMaybe, fromJust)
import Data.Function(on)
import Data.Array(Array, bounds, (!))
import Data.Array.ST.Safe(runSTArray)
import Data.Array.MArray(writeArray, newArray)

-- symbol,exchange,date,adjusted stock close price,option symbol,expiration,strike,call/put,style,ask,bid,volume,open interest,unadjusted stock price
-- SPY,NYSEArca,01/07/05,118.44,SYHXD,12/17/05,160,P,A,0,0,0,0,118.44
data Type = Call | Put
          deriving (Show, Enum)

data Style = American
           deriving Show
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
           deriving Show

data Alg = Alg { spread :: (Double, Double)
               , expired :: Int
               , investRatio :: Double
               , sellThreshold :: Double
               }
         deriving Show
type Options = Array (Day,StrikePrice) (Maybe Quote)
data Account = Account { cash :: Double
                       , puts :: [(Quote, Quote, Int)]
                       , alg :: Alg
                       }
             deriving Show
maxPrice :: Double
maxPrice = 2**32

trade ::  Account -> [Quote] -> Account
trade ac qs = buyPuts os $ sellPuts os ac
    where os = mkArray ls
          ls = map (\ q -> (toix q, q)) qs

mkArray :: [((Day,StrikePrice),Quote)] -> Options
mkArray ls = runSTArray $ do
    let l = fst $ head ls
        h = fst $ last ls
    ar <- newArray (l,h) Nothing
    mapM_ (\ (ix,v) -> writeArray ar ix (Just v)) ls
    return ar

sellValue :: Options -> (Quote,Quote,Int) -> Double
sellValue os (h,l,v)= fromMaybe 0 $ do 
    hp <- bid <$> lookupO os (toix h)
    lp <- ask <$> lookupO os (toix l)
    return $ fromIntegral v * (hp - lp)

toix :: Quote -> (Day, StrikePrice)
toix q = (expiration q, strike q)

maxValue :: (Quote,Quote,Int) -> Double
maxValue (h,l,v) = fromIntegral $ strike h - strike l * v

sellPuts :: Options -> Account -> Account
sellPuts os ac = ac { cash = nc, puts = keep }
    where needsClose (q,_,_) | diffDays (expiration q) d < 7 = True
          needsClose pt | sellValue os pt / maxValue pt > (sellThreshold al) = True
          needsClose pt | sellValue os pt == 0 = True
          needsClose _ = False
          (close,keep) = partition needsClose (puts ac)
          (lb,_) = bounds os
          al = alg ac
          d = date $ fromJust $ os ! lb
          nc = cash ac + (sum $ map (sellValue os) close)

lookupO :: Options -> (Day,StrikePrice) -> Maybe Quote
lookupO os ix = lookup' ix
    where (lo,hi) = bounds os
          lookup' i | i < lo || i > hi = Nothing
          lookup' i | otherwise =  os ! i

buyPuts :: Options -> Account -> Account
buyPuts os ac = fromMaybe ac $ do
    hq <- lookupO os (d,hp)
    lq <- lookupO os (d,lp)
    let bp = (ask hq - bid lq)
        s = investRatio al * c
        v = floor $ s / bp
        nc = c - fromIntegral v * bp
        np = (hq, lq, v) : puts ac
    return $ ac { cash = nc, puts = np }
    where c = cash ac
          p = unadjustedStockPrice (fromJust $ os ! lb)
          (lb,_) = bounds os
          (hi, low) = spread $ alg ac
          hp = (ceiling $ hi * p/5) * 5
          lp = (floor $ low * p/5) * 5
          al = alg ac
          e = fromIntegral $ expired al
          d = addDays e (date $ fromJust $ os ! lb)

parse :: C.ByteString -> [Quote]
parse ln | C.head ln == '#' = []
parse ln = [Quote sm ec (toDay da) (readC ap) os (toDay ex) (readC st) (toType cp) (toStyle sy) (readC as) (readC bi) (readC vo) (readC oi) (readC us)]
    where [sm,ec,da,ap,os,ex,st,cp,sy,as,bi,vo,oi,us] = C.split ',' ln
          toType "C" = Call 
          toType "P" = Put 
          toType e = error $ concat ["toType: unknown string: ", C.unpack e]
          toStyle :: C.ByteString -> Style
          toStyle "A" = American
          toStyle e = error $ concat ["toStyle: unknown string: ", C.unpack e]

readC :: Read a => C.ByteString -> a
readC s = read $ C.unpack s

toDay :: C.ByteString -> Day
toDay str = fromGregorian (readC yy + 2000) (readC mm) (readC dd) 
    where [mm,dd,yy] = C.split '/' str

main :: IO ()
main = do
    let grup = groupBy ((==) `on` expiration)
    quotes <- grup <$> concatMap parse <$> tail <$> C.lines <$> C.readFile "spy_options.1.7.2005.to.12.28.2009.r.csv"
    let al = Alg (0.85, 0.8) 10000 0.1 0.8
    let ac = Account  10000 [] al
    print $ foldl' trade ac  quotes 
