module UtilSpec (main, spec) where

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

  describe "addOne" $ do
    it "appends column of ones to left side of matrix" $ do
      addOnes s `shouldBe` sAddOnes
