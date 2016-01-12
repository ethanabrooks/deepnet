module Main where

import Model
import Util
import Data.Array.Repa hiding (map)
import Debug.Trace

main :: IO ()
main = do
  print "Nothing"
  {-print $ length $ children network-}
  where m1 = matrix
          ([[1, 2]
          , [1, 1]] :: [[Double]])
        m2 = matrix
          ([[0.5, 0.5]
          , [0.5, 0.5]] :: [[Double]])
        layer = sigmoid { input = Just m2 }
        {-layer = linearLayerFromValues ([[1, 2],-}
                                        {-[1, 1]] :: [[Double]])-}
        {-network   = sequentialNet [linearLayer, sigmoid] [2, 2, 2]-}
        {-(_, val) = (backpropogate layer) m1 layer-}

