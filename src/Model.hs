{-# LANGUAGE NamedFieldPuns #-}
{-
- TODO:
- 1. incorporate bias!
- 2. test single layer with sigmoid
- 3. refactor Network class
- 4. add getters and setters to weight, input, etc.
- 5. test train
- 5. regularization
- 7. parallelize
-}

module Model ( linearLayer
             , linearLayerFromMatrix
             , sequentialNet
             , sequentialNetFromMatrices
             , sigmoid
             , sigmoidInput
             , weights
             , input
             , children
             , backprop
             , Network
             , feedThru ) where
import           Util

import           Data.Array.Repa hiding (map, (++), transpose, transpose2S)
import qualified Data.Array.Repa as Repa
import           Data.Array.Repa.Algorithms.Matrix
import           Data.List (foldl')
import           Data.Maybe
import           Control.Monad (join)
import           Prelude hiding (zipWith, sequence)
import Debug.Trace

type Input   = Matrix
type Output  = Matrix
type Targets = Matrix
type Error   = Matrix

class Network a where
    feedThru :: a -> Input -> (a, Output)
    backprop :: a -> Double -> Error -> (a, Error)

class (Network a) => Weighted a where
    getWeights :: a -> Matrix
    gradient   :: a -> Matrix -> Matrix

data (Network a) => Model a =
  Trained     { network      :: a
              , costFunction :: Output -> Targets -> Error
              , learningRate :: Double }

  | Untrained { buildNetwork :: Int -> Int -> a
              , costFunction :: Output -> Targets -> Error
              , learningRate :: Double }

train :: Network a => Input -> Targets -> Model a -> Int -> Model a
train input targets model numEpochs =
    iterate (update input targets) model !! numEpochs

update :: Network a => Input -> Targets -> Model a -> Model a
update input targets model =
    Trained network'' costFunc learnRate
    where (costFunc, learnRate) = (costFunction model, learningRate model)
          network               = getNet input targets model
          (network', output)    = feedThru network input
          error                 = costFunc output targets
          (network'', _)        = backprop network' learnRate error

getNet :: (Network a) => Input -> Targets -> Model a -> a
getNet input targets (Untrained buildNetwork _ _) =
    buildNetwork sizeIn sizeOut
    where Z :. _ :. sizeIn  = extent input
          Z :. _ :. sizeOut = extent targets
getNet _ _ model = network model

addGradients :: (Weighted a) => Double -> Matrix -> a -> Matrix
addGradients learningRate gradient' network =
    getWeights network + rmap (*learningRate) gradient'

sequence :: Network a => (a -> Matrix -> (a, Matrix)) ->
  Matrix -> [a] -> SequentialNet a -> (SequentialNet a, Matrix)
sequence function signal children network =
      (network { children = children' }, out)
      where (children', out) = foldl' propogate ([], signal) $ children
            propogate (children, signal) child = (child':children, signal')
                       where (child', signal') = function child signal

data (Network a) => SequentialNet a = SequentialNet { children :: [a] }

sequentialNet :: (Network a) => (Int -> Int -> a) ->
  [(Int -> Int -> a, Int)] -> Int -> Int -> SequentialNet a
sequentialNet headNet tailNets sizeIn sizeOut = SequentialNet
  { children =
      let build (built, sizeOut) (buildChild, sizeIn) =
            (buildChild sizeIn sizeOut:built, sizeIn)
          (tail, _) = foldl' build ([], sizeOut) $ reverse tailNets
          head = headNet sizeIn sizeOut
      in  head:tail }

instance Network a => Network (SequentialNet a) where
  feedThru network@(SequentialNet children) input =
    (net { children = reverse children }, output)
    where (net, output) = sequence feedThru input children network
  backprop network learningRate error =
    sequence backprop' error (reverse $ children network) network
    where backprop' error = backprop error learningRate

data LinearLayer = LinearLayer { input   :: Maybe Input
                               , weights :: Matrix }

linearLayer :: Int -> Int -> LinearLayer
linearLayer sizeIn sizeOut = LinearLayer
  { input   = Nothing
  , weights = randomArray (sizeIn + 1) sizeOut }


instance Network LinearLayer where
  feedThru layer input =
    (layer { input = Just input' }, input' * (weights layer))
    where input' = addOnes input
  backprop layer learnRate error =
      (layer', error * (transpose $ weights layer))
      where layer'   = layer { weights = weights' }
            weights' = addGradients learnRate (gradient layer error) layer

instance Weighted LinearLayer where
    getWeights           = weights
    gradient layer error = (transpose . ifInitialized $ input layer) * error

data Sigmoid = Sigmoid { sigmoidInput :: Maybe Input }

sigmoid :: Sigmoid
sigmoid = Sigmoid { sigmoidInput = Nothing }

instance Network Sigmoid where
  feedThru sigmoid input = (sigmoid { sigmoidInput = Just input }
                           , rmap (\ x -> 1 / (1 + exp (-x))) input)
  backprop sigmoid _ error = (sigmoid, computeS derivative)
      where activation = ifInitialized $ sigmoidInput sigmoid
            derivative = zipWith (\ e a -> e * a * (1 - a)) error activation
              {-computeS $ error *^ (activation *^ (rmap ((-)1) activation))-}


-- CODE FOR TESTS --

sequentialNetFromMatrices :: [Matrix] -> SequentialNet LinearLayer
sequentialNetFromMatrices matrices =
    network { children = map linearLayerFromMatrix matrices }
    where network = sequentialNet linearLayer [(linearLayer, 2)] 2 2

linearLayerFromValues :: [[Double]] -> LinearLayer
linearLayerFromValues values =
    (linearLayer sizeIn sizeOut) { weights = matrix values }
    where sizeIn  = length values
          sizeOut = length $ values !! 0

linearLayerFromMatrix :: Matrix -> LinearLayer
linearLayerFromMatrix matrix =
    (linearLayer sizeIn sizeOut) { weights = matrix }
    where Z :. sizeIn :. sizeOut = extent matrix

cost :: Output -> Targets -> Error
cost = (-)
