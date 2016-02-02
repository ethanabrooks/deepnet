{-# LANGUAGE NamedFieldPuns #-}
{-
- TODO:
- test single layer with sigmoid
- regularization
- add getters and setters to weight, input, etc.
- test train
- parallelize
-}

module Model (
             {-linearLayer-}
             {-, linearLayerFromMatrix-}
             {-, sigmoid-}
             {-, sigmoidInput-}
             {-, getWeights-}
             {-, input-}
              backprop
             , Layer
             , add
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
data Network = Network (Input -> (Output, Error -> (Network, Error)))

class Layer a where
    feedThru        :: a -> Input -> Output
    backprop        :: a -> Input -> Error -> Error
    update  :: a -> Double -> Error -> (Network, Error)

class (Layer a) => Learner a where
    getWeights  :: a -> Matrix
    setWeights  :: a -> Matrix -> a
    gradient    :: a -> Input -> Error -> Matrix

update' :: Learner a => a -> Double -> Input -> Error -> (a, Error)
update' layer learningRate input error =
  (layer', backprop layer input error)
    where layer'   = setWeights layer weights'
          weights' = addGradients learningRate grad (getWeights layer)
          grad     = gradient layer input error

instance Monoid Network where
    mempty = Network (\ input -> (input, (\ error -> (mempty, error))))
    mappend (Network net1) (Network net2) = Network (\ input ->
      let (output1, backprop1) = net1 input
          (output2, backprop2) = net2 output1
      in  (output2, \ error ->
          let (net2', error2) = backprop2 error
              (net1', error1) = backprop1 error1
          in  (net1' <> net2', error1)))

data Model = Model { network      :: Network
                   , costFunction :: Output -> Targets -> Error
                   , learningRate :: Double }

train :: Input -> Targets -> Model -> Int -> Model
train input targets model numEpochs =
    iterate (epoch input targets) model !! numEpochs

epoch :: Input -> Targets -> Model -> Model
epoch input targets (Model (Network process) costFunction learningRate) =
    Model network' costFunction learningRate
    where (output, backprop) = process input
          error              = costFunction output targets
          (network', _)      = backprop error

add :: Layer a => a -> Double -> Input -> Network
add layer learningRate input = Network (\ input ->
                                       (feedThru layer input, (\ error ->
                                       update layer learningRate error)))

addGradients :: Double -> Matrix -> Matrix -> Matrix
addGradients learningRate gradient weights =
    weights + rmap (*learningRate) gradient

{-instance Functor Signal where-}
    {-fmap = liftM-}

{-instance Applicative Signal where-}
    {-pure input = Signal input (\ error -> (pure, error))-}
    {-(<*>) = ap-}

{-instance Monad Signal where-}
    {-return = pure-}
    {-Signal input backprop >>= f =-}
      {-let Signal output backprop2 = f input-}
      {-in  Signal output (\ error ->-}
        {-let (network2, error2) = backprop2 error-}
            {-(network1, error1) = backprop error2-}
        {-in  (network1 >=> network2, error1))-}


  {-| Untrained { buildLayer :: Int -> Int -> a-}
              {-, costFunction :: Output -> Targets -> Error-}
              {-, learningRate :: Double }-}

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

{-neuralNetLayer :: Layer a => Int -> Int -> SequentialNet a-}
{-neuralNetLayer = sequentialNet linearLayer [sigmoid]-}

{--- sequentialNet linearyLayer [linearLayer 3, linearLayer2]-}


{-instance Layer a => Layer (SequentialNet a) where-}
  {-size = sizeSN-}
  {-feedThru network input =-}
    {-(net { children = reverse $ children net }, output)-}
    {-where (net, output) = sequence feedThru input (children network) network-}
  {-backprop network learningRate error =-}
    {-sequence backprop' error (reverse $ children network) network-}
    {-where backprop' error = backprop error learningRate-}


data LinearLayer = LinearLayer { sizeLinearLayer :: Int
                               , input           :: Maybe Input
                               , weights         :: Matrix }

linearLayer :: Int -> Int -> LinearLayer
linearLayer sizeIn sizeOut = LinearLayer
  { sizeLinearLayer = sizeIn
  , input           = Nothing
  , weights         = randomArray (sizeIn + 1) sizeOut }


instance Layer LinearLayer where
  feedThru layer input = addOnes input * (weights layer)
  backprop layer _ error = error * (transpose $ weights layer)

      {-where layer'   = layer { weights = weights' }-}
            {-weights' = addGradients learnRate (gradient layer error) layer-}

instance Learner LinearLayer where
    getWeights                 = weights
    setWeights layer weights'  = layer { weights = weights' }
    gradient layer input error = (transpose input) * error


data Sigmoid = Sigmoid { sigmoidInput :: Maybe Input
                       , sigmoidSize  :: Int }

sigmoid :: Int -> Sigmoid
sigmoid size = Sigmoid { sigmoidInput = Nothing
                       , sigmoidSize  = size }

instance Layer Sigmoid where
  feedThru sigmoid input = rmap (\ x -> 1 / (1 + exp (-x))) input
  backprop sigmoid input error = computeS derivative
      where derivative = zipWith (\ e i -> e * i * (1 - i)) error input

{-data RecurrentNet a = RecurrentNet { sizeRN :: Int-}
                                   {-, output :: Output-}
                                   {-, core   :: a }-}

{-instance Layer a => Layer (RecurrentNet a) where-}
  {-size = sizeRN-}
  {-feedThru RecurrentNet { output, core } input = feedThru core input-}
    {-where input = computeS $ input ++ output-}
  {-backprop network = backprop core-}

{--- CODE FOR TESTS ---}

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

{-cost :: Output -> Targets -> Error-}
{-cost = (-)-}
