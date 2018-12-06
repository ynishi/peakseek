module LibSpec
  ( spec
  ) where

import           Lib
import           Test.Hspec

spec :: Spec
spec = do
  describe "calcPred" $
    context "when f(x) = 1 + 2x + 3x^2" $ do
      let fx x = 1 + 2 * x + 3 * x * x
      let xs = [0 .. 2]
      let ys = map fx xs
      let d = newDataXY {xs = xs, ys = ys}
      context "when x = 1" $
        it "pred y:6" $
          calcPred 1 d `shouldBe` Pred {xy = XY {x = 1, y = 6.0}}
      context "when x = 2" $
        it "pred y:8.5" $
          calcPred 2 d `shouldBe` Pred {xy = XY {x = 2, y = 17.0}}
      context "when x = 20" $
        it "pred y:1241" $
          calcPred 20 d `shouldBe` Pred {xy = XY {x = 20, y = 1241}}
  describe "calcConst" $
    context "when f(x) = 1 + 2x + 3x^2" $ do
      let fx x = 1 + 2 * x + 3 * x * x
      let xs = [0 .. 9]
      let ys = map fx xs
      let d = newDataXY {xs = xs, ys = ys}
      context "when dim = 3" $
        it "calc const 1,2,3" $
          calcConst 3 d `shouldBe`
            Const {constDataXYId = Just 0, Lib.const = [1, 2, 3]}
