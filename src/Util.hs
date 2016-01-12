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

