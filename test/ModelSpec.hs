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
  describe "Linear Layer" $ do
    let layer = linearLayerFromMatrix m
    it "has correct initial weight values" $ do
      weights layer `shouldBe` m

    it "has no initial input" $ do
      input layer `shouldBe` Nothing

    context "during the forward pass" $ do
      let (layer', output) = feedThru layer m1
      it "updates input" $ do
        input layer' `shouldBe` Just (addOnes m1)
      it "computes output" $ do
        output `shouldBe` (addOnes m1) * m

    context "during backpropogation" $ do
      let (layer', _) = feedThru layer m1
          (layer'', inputError) = backprop layer' 0 m1
      it "processes errors" $ do
        inputError `shouldBe` m1 * transpose m
      context "when learning rate is 0" $ do
        it "updates weights" $ do
          weights layer'' `shouldBe` m
      context "when learning rate is 1" $ do
        it "updates weights" $ do
          let (layer'', inputError) = backprop layer' 1 m1
          weights layer'' `shouldBe` (transpose (addOnes m1) * m1 + m)

  describe "Sigmoid" $ do
    let layer = sigmoid 2
    it "has no initial input" $ do
      sigmoidInput layer `shouldBe` Nothing
    it "feeds forwards" $ do
      let (layer', output) = feedThru layer zeros
      sigmoidInput layer' `shouldBe` Just zeros
      output `shouldBe` rmap (const 0.5) zeros
    it "backpropogates" $ do
      let (layer', _) = feedThru layer m1
          (_, inputError) = backprop layer' 0 m2
      inputError `shouldAlmostEqual` m3

  describe "Sequential Network" $ do
    context "with one layer" $ do
      let network = sequentialNetFromMatrices [m]
          (network', output) = feedThru network m1
      it "has one child" $ do
        (length $ children network) `shouldBe` 1

      context "during the forward pass" $ do
        it "computes output" $ do
          output `shouldBe` (addOnes m1) * m
      it "backpropogates" $ do
        let (network'', inputError) = backprop network' 0 m1
        inputError `shouldBe` m1 * transpose m

    context "with two layers" $ do
      let network = sequentialNetFromMatrices [m1, m2]
          (network', output) = feedThru network m
      it "has two children" $ do
        (length $ children network) `shouldBe` 2

      context "during the forward pass" $ do
        it "computes output" $ do
          output `shouldBe` addOnes ((addOnes m) * m1) * m2
      it "backpropogates" $ do
        let (network'', inputError) = backprop network' 0 m
        inputError `shouldBe` m * transpose m2 * transpose m1
