module Iris.CameraSpec (spec) where

import Test.Hspec

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
      length (buttonMap bs1) `shouldBe` 1
      length (buttonMap bs2) `shouldBe` 1

    let b3 = recordClick (GL.Position 2 2) b Released bs2
    it "removes clicks" $
      length (buttonMap b3) `shouldBe` 0
