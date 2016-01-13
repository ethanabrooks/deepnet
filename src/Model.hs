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
import           Prelude hiding (sequence)
import Debug.Trace

type Input   = Matrix
type Output  = Matrix
type Targets = Matrix
type Error   = Matrix

data Network =
  Network
    { children      :: [Network]
    , input         :: Maybe Input
    , feedThrough   :: Network -> Input -> (Network, Output)
    , backpropogate :: Network -> Double -> Error -> (Network, Error) }

  | Layer
    { weights       :: Matrix
    , input         :: Maybe Input
    , feedThrough   :: Network -> Input -> (Network, Output)
    , backpropogate :: Network -> Double -> Error -> (Network, Error) }

  | NoWeightsLayer
    { input         :: Maybe Input
    , feedThrough   :: Network -> Input -> (Network, Output)
    , backpropogate :: Network -> Double -> Error -> (Network, Error) }

data Model =
  Trained     { network      :: Network
              , costFunction :: Output -> Targets -> Error
              , learningRate :: Double }

  | Untrained { buildNetwork :: Int -> Int -> Network
              , costFunction :: Output -> Targets -> Error
              , learningRate :: Double }

feedThru :: Network -> Input -> (Network, Error)
feedThru net = feedThrough net net

backprop :: Network -> Double -> Error -> (Network, Error)
backprop net = backpropogate net net

train :: Input -> Targets -> Model -> Int -> Model
train input targets model numEpochs =
    iterate (update input targets) model !! numEpochs

update :: Input -> Targets -> Model -> Model
update input targets model =
    Trained network'' costFunc learnRate
    where (costFunc, learnRate)  = (costFunction model, learningRate model)
          network                = getNet input targets model
          (network', output)     = feedThru network input
          error                  = costFunc output targets
          (network'', _)         = backprop network' learnRate error

getNet :: Input -> Targets -> Model -> Network
getNet input targets (Untrained buildNetwork _ _) =
    buildNetwork sizeIn sizeOut
    where Z :. _ :. sizeIn  = extent input
          Z :. _ :. sizeOut = extent targets
getNet _ _ model                                  = network model

instance Show Network where
    show (Network _ children _ _) =
      "{\n  " ++ (concatMap show children) ++ " \n}"
    show _ = show "|"


addGradients :: Network -> Double -> Matrix -> Network
addGradients network learningRate gradient =
  case network of
    Layer weights _ _ _ ->
      network { weights =
                weights + rmap (*learningRate) gradient }
    _ -> error "can only addGradient to individual Layers with weights"

sequence :: (Network -> Matrix -> (Network, Matrix)) ->
  Matrix -> [Network] -> Network -> (Network, Matrix)
sequence function signal children network =
      (network { children = children' }, out)
      where (children', out) = foldl' propogate ([], signal) $ children
            propogate (children, signal) child = (child':children, signal')
                       where (child', signal') = function child signal

sequentialNet :: (Int -> Int -> Network) ->
  [(Int -> Int -> Network, Int)] -> Int -> Int -> Network
sequentialNet headNet tailNets sizeIn sizeOut = Network
  { input = Nothing
  , children =
      let build (built, sizeOut) (buildChild, sizeIn) =
            (buildChild sizeIn sizeOut:built, sizeIn)
          (tail, _) = foldl' build ([], sizeOut) $ reverse tailNets
          head = headNet sizeIn sizeOut
      in  head:tail
  , feedThrough   = \ network input ->
      let (net, output) = sequence feedThru input (children network) network
      in  (net { input = Just input
               , children = reverse $ children net }, output)
  , backpropogate = \ network learningRate error ->
      sequence (\ error -> backprop error learningRate)
        error (reverse $ children network) network
  }

linearLayer :: Int -> Int -> Network
linearLayer sizeIn sizeOut = Layer
  { input            = Nothing
  , weights          = randomArray (sizeIn + 1) sizeOut
  , feedThrough      = \ layer input ->
      let input' = addOnes input
      in  (layer { input = Just input' }, input' * (weights layer))
  , backpropogate    = \ layer learningRate error ->
      let gradient = (transpose . ifInitialized $ input layer) * error
          network  = addGradients layer learningRate gradient
      in (network, error * (transpose $ weights layer))
  }

sigmoid :: Network
sigmoid = NoWeightsLayer
  { input         = Nothing
  , feedThrough   = \ layer input ->
      (sigmoid { input = Just input }
      , rmap (\ x -> 1 / (1 + exp (-x))) input)
  , backpropogate = \ layer _ error ->
      let activation = ifInitialized $ input layer
      in  (sigmoid
          , computeS $ error *^ (activation *^ (rmap ((-)1) activation)))
  }

-- CODE FOR TESTS --

sequentialNetFromMatrices :: [Matrix] -> Network
sequentialNetFromMatrices matrices =
    network { children = map linearLayerFromMatrix matrices }
    where network = sequentialNet linearLayer [(linearLayer, 2)] 2 2

linearLayerFromValues :: [[Double]] -> Network
linearLayerFromValues values =
    (linearLayer sizeIn sizeOut) { weights = matrix values }
    where sizeIn  = length values
          sizeOut = length $ values !! 0

linearLayerFromMatrix :: Matrix -> Network
linearLayerFromMatrix matrix =
    (linearLayer sizeIn sizeOut) { weights = matrix }
    where Z :. sizeIn :. sizeOut = extent matrix

cost :: Output -> Targets -> Error
cost = (-)
