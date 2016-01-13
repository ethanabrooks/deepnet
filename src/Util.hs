{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Util where

import           Data.AEq
import           Data.Array.Repa hiding (map, (++))
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

transpose :: Matrix -> Matrix
transpose = transpose2S

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

-- CODE FOR TESTS --

m :: Matrix
m = matrix
  ([[0.5, 0.25]
  , [0.5, 0.5 ]] :: [[Double]])

m1 :: Matrix
m1 = matrix
  ([[4,  2]
  , [0, -2]] :: [[Double]])

m1dotM :: Matrix
m1dotM = matrix
  ([[ 3,  2]
  , [-1, -1]] :: [[Double]])

m1dotMT :: Matrix
m1dotMT = matrix
  ([[ 2.5, 3]
  , [-0.5,-1]] :: [[Double]])

m1dotM1T :: Matrix
m1dotM1T = matrix
  ([[ 20, -4]
  , [-4,   4]] :: [[Double]])

zeros :: Matrix
zeros = rmap (const 0) m

m2 :: Matrix
m2 = rmap (const 0.1) m

m3 :: Matrix
m3 = matrix
  ([[0.025, 0.01875]
  , [0.025, 0.025  ]] :: [[Double]])

{-sequentialNetWithTarget ::-}
  {-[Network] -> (Int -> Network) -> Targets -> Network-}
{-sequentialNetWithTarget networks lastNetwork targets =-}
    {-sequentialNet $ networks ++ [lastNetwork sizeOut]-}
      {-where Z :. sizeOut :. _ = extent targets-}

{-sequentialChildren :: [(Int -> Int -> Network, Int)] -> [Network]-}
{-sequentialChildren networks =-}
    {-map (\ (network, (sizeIn, sizeOut)) -> network sizeIn sizeOut) networks'-}
    {-where networks' = zip networks $ zip sizes $ tail sizes-}

{-sequentialFeedThrough :: Network -> Input -> (Network, Output)-}
{-sequentialFeedThrough network input =-}
    {-let (newChildren, output) = children network `receive` input-}
        {-receive :: [Network] -> Input -> ([Network], Output)-}
        {-children `receive` input =-}
            {-case children of-}
              {-[]           -> ([], input)-}
              {-(child:rest) ->-}
                 {-let (child', layerOutput) = (feedThrough child) input-}
                     {-(rest', globalOutput) = rest `receive` layerOutput-}
                 {-in  (child':rest', globalOutput)-}
    {-in  (network { input = input-}
                 {-, children = reverse newChildren }-}
        {-, output)-}

{-sequentialBackpropogate :: Network -> Error -> (Network, Error)-}
{-sequentialBackpropogate network error =-}
    {-(network { children = newChildren } , inputError)-}
    {-where (newChildren, inputError) =-}
            {-backprop error $ reverse $ children network-}
          {-backprop :: Error -> [Network] -> ([Network], Error)-}
          {-backprop error children =-}
            {-case children of-}
               {-[]         -> ([], error)-}
               {-child:rest -> (child':rest', finalError)-}
                 {-where (rest', finalError)  = backprop childError rest-}
                       {-(child', childError) = (backpropogate child) error-}


{-zeros :: Int -> Int -> Matrix-}
{-zeros height width =-}
      {-computeS $ fromFunction (Z :. height :. width) const $ fromInteger 0-}

