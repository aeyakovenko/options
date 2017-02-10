module Data.Estimator where

import qualified Statistics.Regression as S
import qualified Data.Vector.Unboxed as V
import qualified Data.Vector.Algorithms.Intro as V
import qualified Data.Vector.Algorithms.Search as V
import qualified Data.Matrix as M


data Estimator = Estimator (V.Vector (Double, Double, Double))

toEstimator :: Double -> Integer -> [Option] -> Estimator
toEstimator p d os = V.sort $ V.fromList $ map fromOption os
    where fromOption o = (expiration o - d, p/strike o, (bid o + ask o) / (2 * p))

estimate (Estimator vs) s d = 
    where ix = V.binarySearch vs (d, p, 0)

solver (x1, y1, z1) (x2, y2, z2) (x3, y3, z3)
    where x1 + y1 = z1
          x2 + y2 = z2
          x3 + y3 = z3
            

solve vs i1 i2 i3 i4 = M.fromLists [[x1,y1,z1]
    where (x1,y1,z1) = vs ! i1
          (x2,y2,z2) = vs ! i2
          (x3,y3,z3) = vs ! i3
          (x4,y4,z4) = vs ! i4

