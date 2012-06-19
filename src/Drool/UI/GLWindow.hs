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


import Data.IORef(IORef, readIORef)
import Data.Array.IO

import Graphics.Rendering.OpenGL as GL

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))

import qualified Graphics.UI.Gtk.OpenGL as GtkGL


import qualified Drool.Types as DT



display :: IORef DT.ContextSettings -> IO ()
display contextSettings = do
  clear [ColorBuffer]
  matrixMode $= Modelview 0
  loadIdentity

  settings <- readIORef contextSettings
  let angle          = DT.angle settings
      hScale         = (DT.scaling settings) / (100.0::GLfloat)
      gridOpacity    = (DT.gridOpacity settings) / (100.0::GLfloat)
      surfOpacity    = (DT.surfaceOpacity settings) / (100.0::GLfloat)
      vscale         = 2.0
      signalLineDist = 0.04::GLfloat

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

  -- [ value_0, ..., value_n ] -> [ Vertex3 index_0 value_0 0, ..., Vertex3 index_n value_n 0 ]
  let toVertexList zVal = zipWith (\i v -> Vertex3 ((fromIntegral i)/(fromIntegral numSamples)*vscale) v (zVal::GLfloat))

  -- Expects a list of samples [GLfloat] and renders them as line:
  let renderSampleLines sampleList = do renderPrimitive LineStrip (
                                          mapM_ GL.vertex (toVertexList 0 [0..numSamples] sampleList) )
                                        GL.translate $ Vector3 0 0 (signalLineDist::GLfloat)

  let tuplesToVertexList idx = zipWith (\i (v1,v2) ->
        let zVal = (fromIntegral idx) * signalLineDist in
          [ Vertex3 ((fromIntegral i)/(fromIntegral numSamples)*vscale) v1 (zVal::GLfloat),
            Vertex3 ((fromIntegral i)/(fromIntegral numSamples)*vscale) v2 (zVal+signalLineDist::GLfloat) ] )
  let renderSampleSurfaceStrip sampleTupleList idx = do color (Color4 0.7 0.2 0.7 surfOpacity :: Color4 GLfloat)
                                                        renderPrimitive TriangleStrip (
                                                          mapM_ GL.vertex (concat(tuplesToVertexList idx [0..numSamples] sampleTupleList)) )
                                                        color (Color4 1 1 1 gridOpacity :: Color4 GLfloat)
                                                        renderPrimitive LineStrip (
                                                          mapM_ GL.vertex (concat(tuplesToVertexList idx [0..numSamples] sampleTupleList)) )

  -- Render grid lines:
{-
  color (Color4 1 1 1 gridOpacity :: Color4 GLfloat)
  preservingMatrix $ do
    -- Render every signal in the buffer as line
    mapM_ (\signal -> do
      samples <- getElems $ DT.signalArray signal
      renderSampleLines samples
      return() )
      (DT.signalList signalBuf)
-}

  let flattenTupleList (x:xs) = case x of
                                  (f,s) -> f : s : flattenTupleList(xs)
                                  _ -> []


  -- Render surface:
  color (Color4 0.7 0.2 0.7 surfOpacity :: Color4 GLfloat)
  let renderSignalSurfaceStrip signals count = (
        case signals of
            (sig:nextSig:xs) -> (
              do samples <- getElems $ DT.signalArray sig
                 nextSamples <- getElems $ DT.signalArray nextSig
                 renderSampleSurfaceStrip (zip samples nextSamples) count
                 -- recurse
                 renderSignalSurfaceStrip(drop 1 signals) (count+1)
                 return () )
            (sig:[]) -> return () )

  renderSignalSurfaceStrip (DT.signalList signalBuf) 0

  GL.flush

reshape allocation = do
  let rectangleWidthHeight (Gtk.Rectangle _ _ w' h') = (w',h')
  let (w,h) = rectangleWidthHeight allocation
  viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
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

  canvas <- GtkGL.glDrawingAreaNew glConfig

  -- Initialise some GL setting just before the canvas first gets shown
  -- (We can't initialise these things earlier since the GL resources that
  -- we are using wouldn't have been setup yet)

  _ <- Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ -> do
    dither $= Enabled
    shadeModel $= Smooth
    blend $= Enabled
    polygonSmooth $= Enabled
    lineSmooth $= Enabled
    blendFunc $= (SrcAlpha, One)
    hint PerspectiveCorrection $= Nicest
    hint PolygonSmooth $= Nicest
    hint LineSmooth $= Nicest

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

