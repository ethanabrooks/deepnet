module Data.String.StripSpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.String.Strip
import Model
import Util

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "linear layer" $ do
    it "does shit" $ do
       1 `shouldBe` 1
      {-where m1 = matrix-}
              {-([[1, 2]-}
              {-, [1, 1]] :: [[Double]])-}
            {-m2 = matrix-}
              {-([[0.5, 0.5]-}
              {-, [0.5, 0.5]] :: [[Double]])-}
            {-layer = sigmoid { input = Just m2 }-}
  describe "strip" $ do
    it "removes leading and trailing whitespace" $ do
      strip "\t  foo bar\n" `shouldBe` "foo bar"
    it "is idempotent" $ property $
      \str -> strip str == strip (strip str)
