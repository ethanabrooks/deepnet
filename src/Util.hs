{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE BangPatterns #-}

module Util where

import           Data.AEq
import           Data.Array.Repa hiding ((++), map, transpose)
import qualified Data.Array.Repa as Repa
import           Data.Array.Repa.Algorithms.Matrix
import           Data.Array.Repa.Algorithms.Randomish
import           Data.List (foldl')
import           Data.Maybe
import           Control.Monad (join)
import           Prelude hiding (sequence)
import qualified Prelude
import           Test.Hspec
import Debug.Trace

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

shouldAlmostEqual :: Matrix -> Matrix -> Expectation
shouldAlmostEqual m1 m2 = shouldSatisfy m1 $ (~==) m2

instance AEq Matrix where
    m1 ~== m2 = sumAllS (m1 - m2) ~== 0

rmap :: (Double -> Double) -> Matrix -> Matrix
rmap f = computeS . Repa.map f

matrix :: [[Double]] -> Matrix
matrix values = fromListUnboxed (Z :. height :. width) $ concat values
  where height = length values
        width  = length $ values !! 0

mean :: Floating a => [a] -> a
mean x = fst $ foldl' (\(!m, !n) x -> (m+(x-m)/(n+1),n+1)) (0,0) x

ifInitialized :: Maybe a -> a
ifInitialized m = case m of Just a  -> a
                            Nothing -> error "Value not initialized"

randomArray :: Int -> Int -> Matrix
randomArray h w = randomishDoubleArray (Z :. h :. w) 0 1 0

addOnes :: Matrix -> Matrix
addOnes m = computeS $ append ones m
  where Z :. height :. _ = extent m
        ones = fromFunction (Z :. height :. 1) $ const 1

transpose :: Matrix -> Matrix
transpose = transpose2S



-- CODE FOR TESTS --

zeros :: Matrix
zeros = rmap (const 0) m

sAddOnes :: Matrix
sAddOnes = matrix
  ([[1, 4,  2]
  , [1, 0, -2]] :: [[Double]])

s = matrix
  ([[4,  2]
  , [0, -2]] :: [[Double]])

m :: Matrix
m = matrix
  ([[0.5, 0.25]
  , [0.5, 0   ]
  , [0.2, 1   ]] :: [[Double]])

m1 :: Matrix
m1 = matrix
  ([[4,  2]
  , [5,  0]
  , [0, -2]] :: [[Double]])

m2 :: Matrix
m2 = rmap (const 0.1) m

m3 :: Matrix
m3 = computeS $ m2 *^ rmap (\ x -> x * (1 - x)) m1
