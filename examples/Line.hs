{-# LANGUAGE MultiWayIf, OverloadedStrings #-}

-- | This is simply a test to make sure things work until I can come up with
-- some real examples.

module Main where

import qualified Data.ByteString as BS
import           Control.Concurrent.STM
import           Control.Monad (unless, when)
import qualified Data.Map.Strict as Map
import qualified Graphics.GLUtil as U
import           Graphics.Rendering.OpenGL (($=))
import qualified Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.GLFW as GLFW
import qualified Linear as L

import qualified Iris.Util.GLFW as W

main :: IO ()
main =
  do win <- W.initialize "Line Plot" (640, 480)

     cameraState <- newTVarIO $ CameraState (L.V2 1 2) 10 7
     mouseState <- newTVarIO $ MouseState (Map.fromList [])
     GLFW.setMouseButtonCallback win $ Just (mouseButtonCallback cameraState mouseState)
     GLFW.setCursorPosCallback win $ Just (cursorPosCallback cameraState mouseState)
     GLFW.setScrollCallback win $ Just (mouseScrollCallback cameraState)

     lineProg <- initLine
     W.mainLoop (draw cameraState lineProg win) win


mouseButtonCallback :: TVar CameraState -> TVar MouseState -> GLFW.MouseButtonCallback
mouseButtonCallback camTVar mouseTVar win button GLFW.MouseButtonState'Pressed  _ =
  do (MouseState buttonMap) <- readTVarIO mouseTVar
     unless (Map.member button buttonMap) $
       do (x, y) <- GLFW.getCursorPos win
          cameraState <- readTVarIO camTVar
          let newMap = Map.insert button (x, y, center cameraState) buttonMap
          atomically $ writeTVar mouseTVar (MouseState newMap)
mouseButtonCallback _ mouseTVar _ button GLFW.MouseButtonState'Released  _ =
  atomically $ modifyTVar' mouseTVar (MouseState . Map.delete button . buttons)

cursorPosCallback :: TVar CameraState -> TVar MouseState -> GLFW.CursorPosCallback
cursorPosCallback camTVar mouse win x y =
  do (w, h) <- GLFW.getWindowSize win
     cameraState <- readTVarIO camTVar
     (MouseState buttonMap) <- readTVarIO mouse
     when (Map.member GLFW.MouseButton'1 buttonMap) $
       do let (ox, oy, ocs) = buttonMap Map.! GLFW.MouseButton'1
              (dx, dy) = (x - ox, y - oy)
              (cw, ch) = (realToFrac $ width cameraState, realToFrac $ height cameraState)
              dxw = realToFrac $ dx * cw / fromIntegral w
              dyw = realToFrac $ dy * ch / fromIntegral h
              c   = ocs + L.V2 (-dxw) dyw
          atomically $ modifyTVar' camTVar (\cs -> cs { center = c })


mouseScrollCallback :: TVar CameraState -> GLFW.ScrollCallback
mouseScrollCallback cam win _ ds =
  do state <- readTVarIO cam
     (xpx, xpy) <- GLFW.getCursorPos win
     (wp, hp) <- GLFW.getWindowSize win
     let f = 1 - 0.1 * ds  -- Zoom factor
         w = realToFrac $ width state
         h = realToFrac $ height state
         w' = w * f
         h' = h * f

         -- Translate center
         (x', y') = mapToWorld (xpx, xpy) (wp, hp) state
         x = L.V2 (realToFrac x') (realToFrac y')
         dx = x - c
         dx' = dx * realToFrac f
         c = center state
         c' = x - dx'

         newState = state { center = c'
                          , width = realToFrac w'
                          , height = realToFrac h' }

     atomically $ writeTVar cam newState

draw :: TVar CameraState -> LineProgram -> GLFW.Window -> IO ()
draw camMVar lp win =
  do GL.clearColor $= GL.Color4 0 0 0 1
     GL.depthFunc $= Just GL.Less
     GL.clear [GL.ColorBuffer, GL.DepthBuffer]

     -- In C++ example GLUT handles this?
     (winWidth, winHeight) <- GLFW.getFramebufferSize win
     GL.viewport $= (GL.Position 0 0,
                     GL.Size (fromIntegral winWidth) (fromIntegral winHeight))

     camState <- readTVarIO camMVar
     let m  = transformM camState
     drawLine lp m

mapToWorld :: (Double, Double) -> (Int, Int) -> CameraState -> (Double, Double)
mapToWorld (xp, yp) (w, h) (CameraState (L.V2 cx cy) cw ch) = (x, y)
  where (w', h')   = (fromIntegral w, fromIntegral h)
        (cxp, cyp) = (w' / 2, h' / 2)  -- Camera center in pixels
        (dxp, dyp) = (xp - cxp, yp - cyp)
        (cw', ch') = (fromRational $ toRational cw, fromRational $ toRational ch)
        (cx', cy') = (fromRational $ toRational cx, fromRational $ toRational cy)
        (dx, dy)   = (dxp * cw' / w', dyp * ch' / h' * (-1))
        (x, y)     = (cx' + dx, cy' + dy) :: (Double, Double)

type CameraCenter = L.V2 GL.GLfloat

data CameraState = CameraState
  { center :: CameraCenter
  , width  :: GL.GLfloat
  , height :: GL.GLfloat
  } deriving (Show)


-- | Stores currently pressed buttons and the coordinates when they were
-- pressed
newtype MouseState = MouseState
  { buttons :: Map.Map GLFW.MouseButton (Double, Double, CameraCenter) }
  deriving (Show)

transformM :: CameraState -> L.M44 GL.GLfloat
transformM (CameraState (L.V2 cx cy) w h) = scale L.!*! trans L.!*! model where
  model = L.identity
  trans = L.V4 (L.V4 1 0 0 (-cx)) (L.V4 0 1 0 (-cy)) (L.V4 0 0 1 0) (L.V4 0 0 0 1)
  scale = L.V4 (L.V4 (2/w) 0 0 0) (L.V4 0 (2/h) 0 0) (L.V4 0 0 1 0) (L.V4 0 0 0 1)


-- Line stuff
data LineProgram = LineProgram U.ShaderProgram GL.BufferObject LineData
type LineData = [L.V2 GL.GLfloat]

lineVerts :: LineData
lineVerts = [ L.V2 1 1
            , L.V2 1 2
            , L.V2 2 2
            ]


initLine :: IO LineProgram
initLine =
  do prog <- U.simpleShaderProgramBS vsSource fsSource
     vbuf <- U.makeBuffer GL.ArrayBuffer lineVerts
     return $ LineProgram prog vbuf lineVerts


drawLine :: LineProgram -> L.M44 GL.GLfloat -> IO ()
drawLine (LineProgram prog vbuf verts) m =
  do GL.currentProgram $= Just (U.program prog)
     U.enableAttrib prog "coord2d"

     GL.bindBuffer GL.ArrayBuffer $= Just vbuf
     U.setAttrib prog "coord2d"
        GL.ToFloat $ GL.VertexArrayDescriptor 2 GL.Float 0 U.offset0
     U.asUniform m $ U.getUniform prog "mvp"
     GL.drawArrays GL.LineStrip 0 (fromIntegral $ length verts)
     GL.vertexAttribArray (U.getAttrib prog "coord2d") $= GL.Disabled


vsSource, fsSource :: BS.ByteString
vsSource = BS.intercalate "\n"
           [
             "attribute vec2 coord2d; "
           , "uniform mat4 mvp;"
           , ""
           , "void main(void) { "
           , "    gl_Position = mvp * vec4(coord2d, 0.0, 1.0); "
           , "}"
           ]

fsSource = BS.intercalate "\n"
           [
             ""
           , "void main(void) { "
           , "    gl_FragColor = vec4(0.0, 0.0, 1.0, 1.0);"
           , "}"
           ]
