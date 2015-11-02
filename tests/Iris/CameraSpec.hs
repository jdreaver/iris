module Iris.CameraSpec (spec) where

import Test.Hspec

import qualified Graphics.Rendering.OpenGL as GL

import Iris.Camera
import Iris.Mouse


spec :: Spec
spec =
  describe "recordClick" $ do
    let buttons = pressedButtons :: PressedButtons PanZoomCamera
        cs      = panZoomCamera
        b       = MouseButtonLeft
        bs1     = recordClick cs b Pressed (GL.Position 0 0) buttons
        bs2     = recordClick cs b Pressed (GL.Position 1 1) bs1

    it "doesn't overwrite clicks" $ do
      length (buttonMap bs1) `shouldBe` 1
      length (buttonMap bs2) `shouldBe` 1

    let b3 = recordClick cs b Released (GL.Position 2 2) bs2
    it "removes clicks" $
      length (buttonMap b3) `shouldBe` 0
