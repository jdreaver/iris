module Iris.CameraSpec (spec) where

import Test.Hspec

import qualified Data.Map.Strict as Map

import Iris.Backends.Class
import Iris.Camera
import Iris.Mouse


spec :: Spec
spec =
  describe "recordClick" $ do
    let buttons = pressedButtons :: PressedButtons
        b       = MouseButtonLeft
        bs1     = recordClick (MousePosition 0 0) b Pressed buttons
        bs2     = recordClick (MousePosition 1 1) b Pressed bs1

    it "doesn't overwrite clicks" $ do
      length (Map.toList bs1) `shouldBe` 1
      length (Map.toList bs2) `shouldBe` 1

    let b3 = recordClick (MousePosition 2 2) b Released bs2
    it "removes clicks" $
      length (Map.toList b3) `shouldBe` 0
