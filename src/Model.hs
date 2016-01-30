{-# LANGUAGE NamedFieldPuns #-}
{-
- TODO:
- test single layer with sigmoid
- regularization
- add getters and setters to weight, input, etc.
- test train
- parallelize
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
{-import           Control.Monad.State.Lazy-}
import           Data.Array.Repa hiding (map,
                                        transpose,
                                        transpose2S,
                                        size)
import qualified Data.Array.Repa as Repa
import           Data.Array.Repa.Algorithms.Matrix
import           Data.List (foldl')
import           Data.Maybe
import           Control.Monad (join)
import           Prelude hiding (sequence, zipWith, (++))
import Debug.Trace

type Input   = Matrix
type Output  = Matrix
type Targets = Matrix
type Error   = Matrix

class Network a where
    size     :: a -> Int
    feedThru :: a -> Input -> (a, Output)
    backprop :: a -> Double -> Error -> (a, Error)

class (Network a) => Weighted a where
    getWeights :: a -> Matrix
    gradient   :: a -> Matrix -> Matrix

data Model a =
  Trained     { network      :: a
              , costFunction :: Output -> Targets -> Error
              , learningRate :: Double }

  | Untrained { buildNetwork :: Int -> Int -> a
              , costFunction :: Output -> Targets -> Error
              , learningRate :: Double }

{-test :: Model -> Input -> Targets -> (Targets -> Output -> Double)-}
{-test model input targets scoreFunc numFolds = mean $ map score folds-}
  {-where score    = scoreFunc (targets, output)-}
        {-output   = model `predict` input-}
        {-folds    = map getFold foldSize input targets [1..numFolds]-}
        {-foldSize = numInstances / numFolds-}
        {-Z :. numInstances :. _ = extent input-}

{-accuracy :: Targets -> Output -> Double-}
{-accuracy targets output = mean $ zipWith equals targets ouput-}
  {-where x `equals` x = 1-}
        {-_ `equals` _ = 0-}

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

data SequentialNet a = SequentialNet { sizeSN   :: Int
                                     , children :: [a] }

sequentialNet :: (Network a) => (Int -> Int -> a) ->
  [Int -> a] -> Int -> Int -> SequentialNet a
sequentialNet head tail sizeIn sizeOut = SequentialNet
  { children = let build (built, sizeOut) buildChild =
                      let child = buildChild sizeOut
                      in  (child:built, size child)
                   (tail', _) = foldl' build ([], sizeOut) $ reverse tail
               in  head sizeIn sizeOut:tail' }


sequence :: Network a => (a -> Matrix -> (a, Matrix)) ->
  Matrix -> [a] -> SequentialNet a -> (SequentialNet a, Matrix)
sequence function signal children network =
      (network { children = children' }, out)
      where (children', out) = foldl' propogate ([], signal) $ children
            propogate (children, signal) child = (child':children, signal')
                       where (child', signal') = function child signal

{-neuralNetLayer :: Network a => Int -> Int -> SequentialNet a-}
{-neuralNetLayer = sequentialNet linearLayer [sigmoid]-}

-- sequentialNet linearyLayer [linearLayer 3, linearLayer2]


instance Network a => Network (SequentialNet a) where
  size = sizeSN
  feedThru network input =
    (net { children = reverse $ children net }, output)
    where (net, output) = sequence feedThru input (children network) network
  backprop network learningRate error =
    sequence backprop' error (reverse $ children network) network
    where backprop' error = backprop error learningRate


data LinearLayer = LinearLayer { sizeLinearLayer :: Int
                               , input           :: Maybe Input
                               , weights         :: Matrix }

linearLayer :: Int -> Int -> LinearLayer
linearLayer sizeIn sizeOut = LinearLayer
  { sizeLinearLayer = sizeIn
  , input           = Nothing
  , weights         = randomArray (sizeIn + 1) sizeOut }


instance Network LinearLayer where
  size = sizeLinearLayer
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


data Sigmoid = Sigmoid { sigmoidInput :: Maybe Input
                       , sigmoidSize  :: Int }

sigmoid :: Int -> Sigmoid
sigmoid size = Sigmoid { sigmoidInput = Nothing
                       , sigmoidSize  = size }

instance Network Sigmoid where
  size = sigmoidSize
  feedThru sigmoid input = (sigmoid { sigmoidInput = Just input }
                           , rmap (\ x -> 1 / (1 + exp (-x))) input)
  backprop sigmoid _ error = (sigmoid, computeS derivative)
      where activation = ifInitialized $ sigmoidInput sigmoid
            derivative = zipWith (\ e a -> e * a * (1 - a)) error activation

data RecurrentNet a = RecurrentNet { sizeRN :: Int
                                   , output :: Output
                                   , core   :: a }

{-instance Network a => Network (RecurrentNet a) where-}
  {-size = sizeRN-}
  {-feedThru RecurrentNet { output, core } input = feedThru core input-}
    {-where input = computeS $ input ++ output-}
  {-backprop network = backprop core-}

-- CODE FOR TESTS --

sequentialNetFromMatrices :: [Matrix] -> SequentialNet LinearLayer
sequentialNetFromMatrices matrices =
    network { children = map linearLayerFromMatrix matrices }
    where network = sequentialNet linearLayer [linearLayer 2] 2 2

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
