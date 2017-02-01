{-# LANGUAGE DeriveGeneric #-}
module Data.YFO( module Data.YFO
               , Quote(Quote)
               , Option(Option)
               , OptionLists(OptionLists)
               ) where

import GHC.Generics
import qualified Data.Aeson as A

import Data.Quote
import Data.OptionLists
import Data.Option


data OptionChain a = OptionChain { optionChain :: a }
                   deriving (Show, Generic)

instance A.FromJSON a => A.FromJSON (OptionChain a)

data Result a = Result { result :: a, error :: Maybe String }
              deriving (Show, Generic)

instance A.FromJSON a => A.FromJSON (Result a)

data OptionsData = OptionsData { underlyingSymbol :: String
                               , expirationDates :: [Integer]
                               , strikes :: [Double]
                               , hasMiniOptions :: Bool
                               , quote :: Quote
                               , options :: [OptionLists]
                               }
                 deriving (Show, Generic)
instance A.FromJSON OptionsData



