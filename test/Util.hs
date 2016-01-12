{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Util where

import           Data.Array.Repa hiding (map, (++))
import qualified Data.Array.Repa as Repa
import           Data.Array.Repa.Algorithms.Matrix
import           Data.Array.Repa.Algorithms.Randomish
import           Data.List (foldl')
import           Data.Maybe
import           Control.Monad (join)
import           Prelude hiding (sequence)
import qualified Prelude

type Vector  = Array U DIM1 Double
type Matrix  = Array U DIM2 Double

instance Num Matrix where
    m1 + m2 = computeS $ m1 +^ m2
    m1 * m2 = m1 `mmultS` m2
    abs m = rmap abs m
    fromInteger n =
      computeS $ fromFunction (Z :. 1 :. 1) . const $ fromInteger n
    negate = rmap negate
    signum = rmap signum

rmap :: (Double -> Double) -> Matrix -> Matrix
rmap f = computeS . Repa.map f

matrix :: [[Double]] -> Matrix
matrix values = fromListUnboxed (Z :. height :. width) $ concat values
  where height = length values
        width  = length $ values !! 0

ifInitialized :: Maybe a -> a
ifInitialized m = case m of Just a  -> a
                            Nothing -> error "Value not initialized"

randomArray :: Int -> Int -> Matrix
randomArray h w = randomishDoubleArray (Z :. h :. w) 0 1 0

{-linearLayerFromValues :: [[Double]] -> Network-}
{-linearLayerFromValues values =-}
    {-(linearLayer sizeIn sizeOut) { weights = matrix values }-}
    {-where sizeIn  = length values-}
          {-sizeOut = length $ values !! 0-}

