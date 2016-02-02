{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-
- TODO:
- test single layer with sigmoid
- regularization
- add getters and setters to weight, input, etc.
- test train
- parallelize
-}

module Model ( linearLayer
             , sigmoid
             , getWeights
             , Network
             , Layer
             , add
             , update
             , getOutError
             , getOutput
             , getNewNetwork
             , (<>)
             , feedThru ) where
import           Util
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
import           Control.Applicative (Applicative(..))
import           Control.Monad       (liftM, ap)
import           Data.Monoid         ((<>))
import           Debug.Trace

type Input   = Matrix
type Output  = Matrix
type Targets = Matrix
type Error   = Matrix
data Network = Network (Input ->
             (Output, Error -> Double ->
             (Error, Network)))

instance Monoid Network where
    mempty = Network (\ input -> (input, (\ error _ -> (error, mempty))))
    mappend (Network net1) (Network net2) = Network (\ input ->
      let (output1, backprop1) = net1 input
          (output2, backprop2) = net2 output1
      in  (output2, \ error learningRate ->
          let (error2, net2') = backprop2 error learningRate
              (error1, net1') = backprop1 error1 learningRate
          in  (error1, net1' <> net2')))

class Layer l where
    feedThru :: l -> Input -> Output
    backprop :: l -> Input -> Error -> Error
    update   :: Double -> Input -> Error -> l -> l
    update _ _ _ = id

class (Layer l) => Learner l where
    getWeights :: l -> Matrix
    setWeights :: l -> Matrix -> l
    gradient   :: l -> Input -> Error -> Matrix

data Model = Model { network      :: Network
                   , costFunction :: Output -> Targets -> Error
                   , learningRate :: Double }

add :: Layer a => a -> Network
add layer = Network process
  where process input = (feedThru layer input, backUpdate)
          where backUpdate error learnRate = (backprop', add layer')
                  where layer'    = update learnRate input error layer
                        backprop' = backprop layer input error

train :: Input -> Targets -> Model -> Int -> Model
train input targets model numEpochs =
    iterate (epoch input targets) model !! numEpochs

epoch :: Input -> Targets -> Model -> Model
epoch input targets (Model (Network process) costFunction learningRate) =
    Model process' costFunction learningRate
    where (output, backprop) = process input
          error              = costFunction output targets
          (_, process')      = backprop error learningRate

addGradients :: Double -> Matrix -> Matrix -> Matrix
addGradients learningRate gradient weights =
    weights + rmap (*learningRate) gradient

updateLearner :: Learner a =>
              Double -> Input -> Error -> a -> a
updateLearner learningRate input error layer = setWeights layer weights
      where weights = addGradients learningRate grad (getWeights layer)
            grad    = gradient layer input error

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

{-neuralNetLayer :: Layer a => Int -> Int -> SequentialNet a-}
{-neuralNetLayer = sequentialNet linearLayer [sigmoid]-}

data LinearLayer = LinearLayer { sizeLinearLayer :: Int
                               , weights         :: Matrix }

linearLayer :: Matrix -> LinearLayer
linearLayer weights = LinearLayer { weights = weights }

instance Layer LinearLayer where
  feedThru layer input   = addOnes input * (weights layer)
  backprop layer _ error = error * (transpose $ weights layer)
  update = updateLearner

instance Learner LinearLayer where
    getWeights                 = weights
    setWeights layer weights'  = layer { weights = weights' }
    gradient layer input error = (transpose input) * error


data Sigmoid = Sigmoid

sigmoid :: () -> Sigmoid
sigmoid () = Sigmoid

instance Layer Sigmoid where
  feedThru sigmoid input       = rmap (\ x -> 1 / (1 + exp (-x))) input
  backprop sigmoid input error = computeS derivative
      where derivative = zipWith (\ e i -> e * i * (1 - i)) error input

{--- CODE FOR TESTS ---}
getOutput :: Layer a => (params -> a) -> params -> Input -> Output
getOutput getLayer params input = fst $ process input
  where Network process = add $ getLayer params

getErrorBackUpdate :: Layer a =>
  (params -> a) -> params -> Input -> Error -> Double -> (Error, Network)
getErrorBackUpdate getLayer params input = snd $ process input
  where Network process = add $ getLayer params

getOutError :: Layer a => (params -> a) -> params -> Input -> Error -> Error
getOutError getLayer params input error = fst $ back error 0
    where (_, back)       = process input
          Network process = add $ getLayer params

getNewNetwork :: Layer a =>
  (params -> a) -> params -> Input -> Error -> Double -> Network
getNewNetwork getLayer params input error learnRate =
    snd $ back error learnRate
    where (_, back)       = process input
          Network process = add $ getLayer params

getNewLayer :: Learner a =>
  (params -> a) -> params -> Input -> Error -> Double -> a
getNewLayer getLayer params input error learnRate =
    update learnRate input error $ getLayer params

{--- sequentialNet linearyLayer [linearLayer 3, linearLayer2]-}


{-instance Layer a => Layer (SequentialNet a) where-}
  {-size = sizeSN-}
  {-feedThru network input =-}
    {-(net { children = reverse $ children net }, output)-}
    {-where (net, output) = sequence feedThru input (children network) network-}
  {-backprop network learningRate error =-}
    {-sequence backprop' error (reverse $ children network) network-}
    {-where backprop' error = backprop error learningRate-}
{-getNet :: (Layer a) => Input -> Targets -> Model a -> a-}
{-getNet input targets (Untrained buildLayer _ _) =-}
    {-buildLayer sizeIn sizeOut-}
    {-where Z :. _ :. sizeIn  = extent input-}
          {-Z :. _ :. sizeOut = extent targets-}
{-getNet _ _ model = network model-}

{-data SequentialNet a = SequentialNet { sizeSN   :: Int-}
                                     {-, children :: [a] }-}

{-sequentialNet :: (Layer a) => (Int -> Int -> a) ->-}
  {-[Int -> a] -> Int -> Int -> SequentialNet a-}
{-sequentialNet head tail sizeIn sizeOut = SequentialNet-}
  {-{ children = let build (built, sizeOut) buildChild =-}
                      {-let child = buildChild sizeOut-}
                      {-in  (child:built, size child)-}
                   {-(tail', _) = foldl' build ([], sizeOut) $ reverse tail-}
               {-in  head sizeIn sizeOut:tail' }-}


{-sequence :: Layer a => (a -> Matrix -> (a, Matrix)) ->-}
  {-Matrix -> [a] -> SequentialNet a -> (SequentialNet a, Matrix)-}
{-sequence function signal children network =-}
      {-(network { children = children' }, out)-}
      {-where (children', out) = foldl' propogate ([], signal) $ children-}
            {-propogate (children, signal) child = (child':children, signal')-}
                       {-where (child', signal') = function child signal-}
{-data RecurrentNet a = RecurrentNet { sizeRN :: Int-}
                                   {-, output :: Output-}
                                   {-, core   :: a }-}

{-instance Layer a => Layer (RecurrentNet a) where-}
  {-size = sizeRN-}
  {-feedThru RecurrentNet { output, core } input = feedThru core input-}
    {-where input = computeS $ input ++ output-}
  {-backprop network = backprop core-}


{-sequentialNetFromMatrices :: [Matrix] -> SequentialNet LinearLayer-}
{-sequentialNetFromMatrices matrices =-}
    {-network { children = map linearLayerFromMatrix matrices }-}
    {-where network = sequentialNet linearLayer [linearLayer 2] 2 2-}

{-linearLayerFromValues :: [[Double]] -> LinearLayer-}
{-linearLayerFromValues values =-}
    {-(linearLayer sizeIn sizeOut) { weights = matrix values }-}
    {-where sizeIn  = length values-}
          {-sizeOut = length $ values !! 0-}

{-linearLayerFromMatrix :: Matrix -> LinearLayer-}
{-linearLayerFromMatrix matrix =-}
    {-(linearLayer sizeIn sizeOut) { weights = matrix }-}
    {-where Z :. sizeIn :. sizeOut = extent matrix-}


{-getOutError :: Layer a => a -> params -> Double -> Input -> Error -> Error-}
{-getOutError layer params learningRate input error = error'-}
  {-where (backprop, _) = getErrorBackUpdate layer params learningRate input-}


{-cost :: Output -> Targets -> Error-}
{-cost = (-)-}
