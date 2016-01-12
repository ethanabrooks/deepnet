{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module TestUtil where

import           Model
import           Util

import           Data.Array.Repa hiding (map, (++))
import qualified Data.Array.Repa as Repa
import           Data.Array.Repa.Algorithms.Matrix
import           Data.Array.Repa.Algorithms.Randomish

linearLayerFromValues :: [[Double]] -> Network
linearLayerFromValues values =
    (linearLayer sizeIn sizeOut) { weights = matrix values }
    where sizeIn  = length values
          sizeOut = length $ values !! 0

{-instance Eq Matrix where-}
    {-(==) = Repa.equalsS-}

m :: Matrix
m = matrix ([[1,  2]
           , [0, -1]] :: [[Double]])

m1 :: Matrix
m1 = matrix ([[1, 2]
            , [1, 1]] :: [[Double]])

m2 :: Matrix
m2 = matrix ([[0.5, 0.2]
            , [0.5, 0.5]] :: [[Double]])
