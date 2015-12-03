module Iris.CameraSpec (spec) where

import Test.Hspec

import qualified Data.Map.Strict as Map
import qualified Graphics.Rendering.OpenGL as GL

import Iris.Camera
import Iris.Mouse


spec :: Spec
spec =
  describe "recordClick" $ do
    let buttons = pressedButtons :: PressedButtons
        b       = MouseButtonLeft
        bs1     = recordClick (GL.Position 0 0) b Pressed buttons
        bs2     = recordClick (GL.Position 1 1) b Pressed bs1

    it "doesn't overwrite clicks" $ do
      length (Map.toList bs1) `shouldBe` 1
      length (Map.toList bs2) `shouldBe` 1

    let b3 = recordClick (GL.Position 2 2) b Released bs2
    it "removes clicks" $
      length (Map.toList b3) `shouldBe` 0
