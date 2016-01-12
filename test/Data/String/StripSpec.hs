module Data.String.StripSpec (main, spec) where

import Model
import TestUtil

import Test.Hspec
import Test.QuickCheck
import Data.String.Strip

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec

spec :: Spec
spec = do
  describe "Matrix" $ do
    it "implements equality" $ do
      m `shouldBe` m
    it "implements inequality" $ do
      m1 `shouldNotBe` m2

{-layer = sigmoid { input = Just m2 }-}
