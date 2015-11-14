{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Defines a triangle item for plots

module Iris.Triangle
       ( TriangleItem (..)
       , TriangleVertices

       , TriangleSpec (..)
       , triangleSpec
       , triangleInit
       , triangleNode
       ) where

import           Control.Lens
import qualified Graphics.GLUtil as U
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Linear as L
import           Reactive.Banana
import           Reactive.Banana.Frameworks

import Iris.Colors
import Iris.Line (fsSource, vsSource)
import Iris.Reactive
import Iris.SceneGraph
import Iris.Transformation

type TriangleVertices = [L.V2 GL.GLfloat]

-- | Shader program and buffer objects for a triangle
data TriangleItem = TriangleItem U.ShaderProgram GL.BufferObject TriangleVertices Color

-- | Reactive version of TriangleItem
data ReactiveTriangleItem = ReactiveTriangleItem
  { _reactiveTriangleItemProgram  :: U.ShaderProgram
  , _reactiveTriangleItemBuffer   :: Observable GL.BufferObject
  , _reactiveTriangleItemVerts    :: Subject [L.V2 GL.GLfloat]
  , _reactiveTriangleItemColor    :: Color
  }

data TriangleSpec = TriangleSpec
  { _TriangleSpecVertices :: [L.V2 GL.GLfloat]
  , _TriangleSpecColors   :: Color
  }

triangleSpec :: TriangleSpec
triangleSpec = TriangleSpec [] (L.V3 1 1 1)

makeFields ''ReactiveTriangleItem


triangleInit :: TriangleSpec -> MomentIO ReactiveTriangleItem
triangleInit (TriangleSpec verts' c) =
  do prog <- liftIO $ U.simpleShaderProgramBS vsSource fsSource
     vs   <- subject verts'
     vbuf <- triangleBuff vs
     return $ ReactiveTriangleItem prog vbuf vs c

triangleBuff :: Subject [L.V2 GL.GLfloat] ->          -- ^ Input vertices subject
                MomentIO (Observable GL.BufferObject) -- ^ Resulting buffer observable
triangleBuff s = mapObservableIO (asObservable s) makeBuffer
  where makeBuffer :: [L.V2 GL.GLfloat] -> IO GL.BufferObject
        makeBuffer = U.makeBuffer GL.ArrayBuffer

triangleNode :: ReactiveTriangleItem -> Visual
triangleNode item = Visual f
  where f :: Event () -> Behavior Transformation -> MomentIO ()
        f eDraw bTrans = do let bData = (,) <$> bTriangleItem item <*> bTrans
                                eData = bData <@ eDraw
                            reactimate $ uncurry drawTriangle <$> eData

bTriangleItem :: ReactiveTriangleItem -> Behavior TriangleItem
bTriangleItem item = TriangleItem <$> pure (item ^. program)
                                  <*> item ^. buffer ^. behavior
                                  <*> item ^. verts ^. behavior
                                  <*> pure (item ^. color)


-- | Draw a given triangle program to the current OpenGL context
drawTriangle :: TriangleItem -> L.M44 GL.GLfloat -> IO ()
drawTriangle (TriangleItem prog vbuf verts' color') m =
  do GL.currentProgram $= Just (U.program prog)
     U.enableAttrib prog "coord2d"

     GL.bindBuffer GL.ArrayBuffer $= Just vbuf
     U.setAttrib prog "coord2d"
        GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0
     U.asUniform m $ U.getUniform prog "mvp"
     U.asUniform color' $ U.getUniform prog "f_color"

     GL.drawArrays GL.Triangles 0 (fromIntegral $ length verts')

     GL.vertexAttribArray (U.getAttrib prog "coord2d") $= GL.Disabled
