{-# LANGUAGE DeriveGeneric #-}
module Data.OptionLists where

import GHC.Generics
import qualified Data.Aeson as A

import Data.Option

data OptionLists = OptionLists  { expirationDate :: Integer
                                , hasMiniOptions :: Bool
                                , calls :: [Option]
                                , puts :: [Option]
                                }
                 deriving (Show, Generic)
instance A.FromJSON OptionLists

