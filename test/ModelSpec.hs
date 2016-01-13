module ModelSpec (main, spec) where

import Model
import Util

import Data.AEq
import Test.Hspec
import Test.QuickCheck

import Debug.Trace

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "Matrix" $ do
    it "implements equality" $ do
      m `shouldBe` m
    it "implements inequality" $ do
      m1 `shouldNotBe` m

  describe "Linear Layer" $ do
    let layer = linearLayerFromMatrix m
    it "has the correct initial weight values" $ do
      weights layer `shouldBe` m

    it "has no initial input" $ do
      input layer `shouldBe` Nothing

    it "feeds forward" $ do
      let (layer', output) = feedThru layer m1
      input layer' `shouldBe` Just m1
      output `shouldBe` m1dotM

    describe "backpropogates" $ do
      let layer' = layer { input = Just m1 }
          (layer'', inputError) = backprop layer' 0 m1
      it "processing errors" $ do
        inputError `shouldBe` m1 * transpose m
      it "updating weights" $ do
        weights layer'' `shouldBe` m
        let (layer'', inputError) = backprop layer' 1 m1
        weights layer'' `shouldBe` (m1 * transpose m1 + m)

  describe "Sigmoid" $ do
    let layer = sigmoid
    it "has no initial input" $ do
      input layer `shouldBe` Nothing
    it "feeds forwards" $ do
      let (layer', output) = feedThru layer zeros
      input layer' `shouldBe` Just zeros
      output `shouldBe` rmap (const 0.5) zeros
    it "backpropogates" $ do
      let layer' = layer { input = Just m }
          (_, inputError) = backprop layer' 0 m2
      inputError `shouldAlmostEqual` m3

  describe "Sequential Network" $ do
    let network = sequentialNetFromMatrices [m1, m2]
        (network', output) = feedThru network m
    it "has no initial input" $ do
      input network `shouldBe` Nothing
    it "has two children" $ do
      (length $ children network) `shouldBe` 2
    it "feeds forward" $ do
      output `shouldBe` m * m1 * m2
      input network' `shouldBe` Just m
    it "backpropogates" $ do
      let (network'', inputError) = backprop network' 0 m
      inputError `shouldBe` m * transpose m2 * transpose m1
