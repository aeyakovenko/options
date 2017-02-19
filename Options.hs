{-# LANGUAGE OverloadedStrings #-}
module Options where
import qualified Data.ByteString.Lazy.Char8 as C
import Data.List(foldl', partition, groupBy)
import Data.Time.Calendar(Day, fromGregorian, addDays, diffDays)
import Data.Maybe(fromMaybe, fromJust, isJust)
import Data.Function(on)
import Data.Array(Array, bounds, (!))
import Data.Array.ST.Safe(runSTArray)
import Data.Array.MArray(writeArray, newArray, getBounds)
import Debug.Trace(traceShowId)

-- symbol,exchange,date,adjusted stock close price,option symbol,expiration,strike,call/put,style,ask,bid,volume,open interest,unadjusted stock price
-- SPY,NYSEArca,01/07/05,118.44,SYHXD,12/17/05,160,P,A,0,0,0,0,118.44
data Type = Call
          | Put
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
                       , valuation :: Double
                       }
             deriving Show

trade ::  Account -> [Quote] -> Account
trade ac qs = updateValuation os $ buyPuts q os $ sellPuts q os ac
    where os = mkArray ls
          q = head qs
          ls = map (\ q -> (toix q, q)) ps
          ps = filter isPut qs
          isPut (Quote { callOrPut = Put }) = True
          isPut _ = False

mkArray :: [((Day,StrikePrice),Quote)] -> Options
mkArray ls = runSTArray $ do
    let a = fst $ fst $ head ls
        b = fst $ fst $ last ls
        ld = a `min` b
        hd = a `max` b
        lp = 0
        hp = 300
    ar <- newArray ((ld,lp),(hd,hp)) Nothing
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

sellPuts :: Quote -> Options -> Account -> Account
sellPuts q os ac = ac { cash = nc, puts = keep }
    where needsClose (qo,_,_) | diffDays (expiration qo) d < 14 = True
          needsClose pt | sellValue os pt / maxValue pt > (sellThreshold al) = True
          needsClose pt | sellValue os pt <= 0 = True
          needsClose _ = False
          (close,keep) = partition needsClose (puts ac)
          al = alg ac
          d = date q
          nc = cash ac + (sum $ map (sellValue os) close)

lookupO :: Options -> (Day,StrikePrice) -> Maybe Quote
lookupO os ix = lookup' ix
    where (lo,hi) = bounds os
          lookup' i | i < lo || i > hi = Nothing
          lookup' i | otherwise =  os ! i

updateValuation :: Options -> Account -> Account
updateValuation os ac = ac { valuation = c + v }
    where v = sum $ map (sellValue os) (puts ac)
          c = cash ac

buyPuts :: Quote -> Options -> Account -> Account
buyPuts q os ac | (cash ac) <= 0 = ac
buyPuts q os ac = fromMaybe ac $ do
    d <- findDay os st (hp,lp)
    hq <- lookupO os (d,hp)
    lq <- lookupO os (d,lp)
    let bp = (ask hq - bid lq)
        s = investRatio al * c
        v = (floor $ s / bp) `max` 0
        nc = c - fromIntegral v * bp
        np = (hq, lq, v) : puts ac
    return $ ac { cash = nc, puts = np }
    where c = cash ac
          p = unadjustedStockPrice q
          (hi, low) = spread $ alg ac
          hp = (ceiling $ hi * p/5) * 5
          lp = (floor $ low * p/5) * 5
          al = alg ac
          e = fromIntegral $ expired al
          cd = date q
          st = addDays e cd

findDay :: Options -> Day -> (Int,Int) -> Maybe Day
findDay ar st (hp,lp) | (snd $ bounds ar) < (st,hp) = Nothing
                      | (fst $ bounds ar) > (st,lp) = Nothing
                      | isJust (ar ! (st, hp)) && isJust (ar ! (st, lp)) = Just st
                      | otherwise = findDay ar (addDays 1 st) (hp,lp)

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
    let grup = groupBy ((==) `on` date)
    quotes <- grup <$> concatMap parse <$> tail <$> C.lines <$> C.readFile "spy_options.1.7.2005.to.12.28.2009.r.csv"
    let al = Alg (0.85, 0.8) 300 0.001 0.45
    let ac = Account  100000 [] al 0
    print $ foldl' trade ac  quotes 
