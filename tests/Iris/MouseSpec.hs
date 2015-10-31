module Iris.MouseSpec (spec) where

import Test.Hspec

--import qualified Data.Map as Map
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L

import Iris.Mouse


spec :: Spec
spec =
  describe "recordClick" $ do
    let buttons = pressedButtons
        center  = L.V2 1.1 2.2
        b       = MouseButtonLeft
        b1      = recordClick center b Pressed (GL.Position 0 0) buttons
        b2      = recordClick center b Pressed (GL.Position 1 1) b1

    it "doesn't overwrite clicks" $ do
      length (buttonMap b1) `shouldBe` 1
      length (buttonMap b2) `shouldBe` 1

    let b3 = recordClick center b Released (GL.Position 2 2) b2
    it "removes clicks" $
      length (buttonMap b3) `shouldBe` 0
