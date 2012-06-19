-----------------------------------------------------------------------------
--
-- Module      :  Drool.UI.GLWindow
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

{-# OPTIONS -O2 -Wall #-}

module Drool.UI.GLWindow (
    initComponent
) where


-- import Control.Monad (unless)
-- import Data.List (stripPrefix)
-- import System.Exit (exitFailure)
import Data.IORef(IORef, readIORef)
import Data.Array.IO

import Graphics.Rendering.OpenGL as GL

-- import Graphics.UI.Gtk.Abstract.Widget as GtkAbstractWidget
-- import Graphics.UI.Gtk.Builder as GtkBuilder

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))

-- import Graphics.UI.GLUT(ReshapeCallback,reshapeCallback)
import qualified Graphics.UI.Gtk.OpenGL as GtkGL


import qualified Drool.Types as DT



display :: IORef DT.ContextSettings -> IO ()
display contextSettings = do
  clear [ColorBuffer]
  matrixMode $= Modelview 0
  loadIdentity

  settings <- readIORef contextSettings
  let angle  = DT.angle settings
  let hScale = (DT.scaling settings) / (100.0::GLfloat)
  let vscale = 2.0
  let signalLineDist = 0.04::GLfloat

  -- Rotate/translate to change view perspective to isometric
  case DT.renderPerspective settings of
    DT.Isometric -> do
      GL.translate $ Vector3 0 0.1 (-1.7::GLfloat)
      GL.rotate (45::GLfloat) $ Vector3 1.0 0.0 0.0
      GL.rotate (45::GLfloat) $ Vector3 0.0 1.0 0.0
      GL.rotate angle $ Vector3 0.0 1.0 0.0
    DT.Top -> do
      GL.translate $ Vector3 0 0 (-1.8::GLfloat)
      GL.rotate (90::GLfloat) $ Vector3 1.0 0.0 0.0
    DT.Front -> do
      GL.translate $ Vector3 0 0 (-2.0::GLfloat)
    DT.Side -> do
      GL.translate $ Vector3 0 0 (-2.0::GLfloat)
      GL.rotate (-90::GLfloat) $ Vector3 0.0 1.0 0.0


  GL.translate $ Vector3 (-0.5 * vscale) 0 0

  GL.scale 1 hScale (1::GLfloat)

  -- Load signal buffer from context
  signalBuf <- readIORef (DT.signalBuf settings)

  let numSignals = length $ DT.signalList signalBuf

  -- Load first signal from buffer to get its length
  let firstSignal = DT.getSignal signalBuf 0
  firstSignalBounds <- getBounds $ DT.signalArray firstSignal
  let numSamples = rangeSize firstSignalBounds

  GL.translate $ Vector3 0 0 (-(fromIntegral numSignals) * signalLineDist / 2.0)

  -- List of all signals in buffer [ Signal_0, ... , Signal_n ]
  -- signalList <- getElems $ DT.signalBufferArray signalBuf
  -- let signalList = signalBuf

  -- values -> [ Vertex3 index_0 value_0 0, ..., Vertex3 index_n value_n 0 ]
  let toVertexList = zipWith (\i v -> Vertex3 ((fromIntegral i)/(fromIntegral numSamples)*vscale) v (0::GLfloat))

  -- Expects a list of samples [GLfloat] and renders them as line:
  let renderSamples sampleList = do color (Color4 1 1 1 0.5 :: Color4 GLfloat)
                                    renderPrimitive LineStrip $ mapM_ GL.vertex (toVertexList [0..numSamples] sampleList)
                                    -- render base line
                                    -- color (Color3 0.5 0.5 1 :: Color3 GLfloat)
                                    -- renderPrimitive LineStrip $ mapM_ vertex [
                                    --   Vertex3 0.0 0 0.0, Vertex3 (1.0*vscale) 0 (0.0::GLfloat) ]
                                    GL.translate $ Vector3 0 0 (signalLineDist::GLfloat)

  -- Render every signal in the buffer
  mapM_ (\signal -> do
    samples <- getElems $ DT.signalArray signal
    renderSamples samples
    return() )
    (DT.signalList signalBuf)

  GL.flush


reconfigure :: Int -> Int -> IO (Int, Int)
reconfigure w h = do
  -- maintain aspect ratio
  let aspectRatio = (fromIntegral canvasWidth) / (fromIntegral canvasHeight)
      (w1, h1)    = (fromIntegral w, (fromIntegral w) / aspectRatio)
      (w2, h2)    = ((fromIntegral h) * aspectRatio, fromIntegral h)
      (w', h')    = if h1 <= fromIntegral h
                      then (floor w1, floor h1)
                      else (floor w2, floor h2)
  reshape $ Just (w', h')
  return (w', h')

reshape allocation = do
  let width  = canvasWidth
  let height = canvasHeight
  viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
  matrixMode $= Modelview 0
  return ()


initComponent _ contextSettings = do

  window <- Gtk.windowNew

  Gtk.set window [ Gtk.containerBorderWidth := 8,
                   Gtk.windowTitle := "drool visualizer" ]

  putStrLn "Initializing OpenGL viewport"

  glConfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA, GtkGL.GLModeMultiSample,
                                 GtkGL.GLModeDouble, GtkGL.GLModeDepth, GtkGL.GLModeAlpha]
  _ <- GtkGL.initGL

  dither $= Enabled
  shadeModel $= Smooth
  blend $= Enabled
  hint PerspectiveCorrection $= Nicest
  hint PolygonSmooth $= Nicest
  hint LineSmooth $= Nicest

  -- blendFunc $= GL.SourceAlpha GL.OneMinusSourceAlpha

  polygonSmooth $= Enabled
  lineSmooth $= Enabled

  canvas <- GtkGL.glDrawingAreaNew glConfig

  -- Initialise some GL setting just before the canvas first gets shown
  -- (We can't initialise these things earlier since the GL resources that
  -- we are using wouldn't have been setup yet)

  _ <- Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ -> do
    -- _ <- reconfigure canvasWidth canvasHeight
    putStrLn "onRealize canvas"
    matrixMode $= Projection
    loadIdentity
    viewport $= (Position 0 0, Size (fromIntegral 800) (fromIntegral 800))
    perspective 90 (fromIntegral 800 / fromIntegral 800) 0.1 100
    matrixMode $= Modelview 0
    loadIdentity
    return ()

  -- OnShow handler for GL canvas:
  _ <- Gtk.onExpose canvas $ \_ -> do
    GtkGL.withGLDrawingArea canvas $ \glwindow -> do
      GL.clear [GL.DepthBuffer, GL.ColorBuffer]
      display contextSettings
      GtkGL.glDrawableSwapBuffers glwindow
    return True

  -- Resize handler:
  _ <- Gtk.onSizeAllocate canvas (reshape)

  -- Add canvas (OpenGL drawing area) to GUI:
  Gtk.widgetSetSizeRequest canvas canvasWidth canvasHeight

  Gtk.set window [ Gtk.containerChild := canvas ]

  -- Redraw canvas every 20ms:
  updateCanvasTimer <- Gtk.timeoutAddFull (do
      Gtk.widgetQueueDraw canvas
      return True)
    Gtk.priorityDefaultIdle 20

  -- Remove timer for redrawing canvas when closing window:
  _ <- Gtk.onDestroy window (Gtk.timeoutRemove updateCanvasTimer)

  Gtk.widgetShowAll window

canvasWidth :: Int
canvasWidth = 800
canvasHeight :: Int
canvasHeight = 800

