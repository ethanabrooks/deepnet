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
    it "computes output" $ do
      let output = getOutput linearLayer m m1
      output `shouldBe` (addOnes m1) * m

    context "during backpropogation" $ do
      {-let (layer', _) = feedThru layer m1-}
          {-(layer'', inputError) = backprop layer' 0 m1-}
      {-let (output, backprop) = add (layer m1) 0 m1-}
          {-(layer', outError) = backprop m1-}
      it "processes errors" $ do
        let outError = getOutError linearLayer m m1 m1
        outError `shouldBe` m1 * transpose m
      context "when learning rate is 0" $ do
        it "updates weights" $ do
          let weights = update (linearLayer m) m1 m
          weights `shouldBe` m
          {-getWeights layer' `shouldBe` m-}
      {-context "when learning rate is 1" $ do-}
        {-it "updates weights" $ do-}
          {-let (output, backprop) = add layer 1 m1-}
              {-(layer', outError) = backprop m1-}
          {-let (layer'', inputError) = backprop layer' 1 m1-}
          {-getWeights layer' `shouldBe` (transpose (addOnes m1) * m1 + m)-}

  describe "Sigmoid" $ do
    let layer = sigmoid ()
    {-it "has no initial input" $ do-}
      {-sigmoidInput layer `shouldBe` Nothing-}
    it "feeds forwards" $ do
      let output = getOutput sigmoid () zeros
      {-sigmoidInput layer' `shouldBe` Just zeros-}
      output `shouldBe` rmap (const 0.5) zeros
    it "backpropogates" $ do
      let outError = getOutError sigmoid () m1 m2
      {-let (layer', _) = feedThru layer m1-}
          {-(_, inputError) = backprop layer' 0 m2-}
      outError `shouldAlmostEqual` m3

  {-describe "Sequential Network" $ do-}
    {-context "with one layer" $ do-}
      {-let network = sequentialNetFromMatrices [m]-}
          {-(network', output) = feedThru network m1-}
      {-it "has one child" $ do-}
        {-(length $ children network) `shouldBe` 1-}

      {-context "during the forward pass" $ do-}
        {-it "computes output" $ do-}
          {-output `shouldBe` (addOnes m1) * m-}
      {-it "backpropogates" $ do-}
        {-let (network'', inputError) = backprop network' 0 m1-}
        {-inputError `shouldBe` m1 * transpose m-}

    {-context "with two layers" $ do-}
      {-let network = sequentialNetFromMatrices [m1, m2]-}
          {-(network', output) = feedThru network m-}
      {-it "has two children" $ do-}
        {-(length $ children network) `shouldBe` 2-}

      {-context "during the forward pass" $ do-}
        {-it "computes output" $ do-}
          {-output `shouldBe` addOnes ((addOnes m) * m1) * m2-}
      {-it "backpropogates" $ do-}
        {-let (network'', inputError) = backprop network' 0 m-}
        {-inputError `shouldBe` m * transpose m2 * transpose m1-}
