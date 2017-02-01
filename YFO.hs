module YFO where

import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as L
import qualified Data.YFO as Y

-- URL = "https://query2.finance.yahoo.com/v7/finance/options/SPY?formatted=true&lang=en-US&region=US&date=1516320000&straddle=false&corsDomain=finance.yahoo.com"

parse :: L.ByteString -> Y.OptionChain (Y.Result [Y.OptionsData])
parse = from . A.eitherDecode
    where
        from (Left err) = error err
        from (Right v) = v

main :: IO ()
main = do
    f <- L.readFile "test.json"
    print $ parse f
