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

import Control.Monad ( foldM ) 

import Data.IORef(IORef, readIORef)
import Data.Array.IO

import Graphics.Rendering.OpenGL as GL

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))

import qualified Graphics.UI.Gtk.OpenGL as GtkGL

import qualified Drool.Utils.Conversions as Conv
import qualified Drool.Utils.RenderHelpers as RH
import qualified Drool.Types as DT
import qualified Drool.ApplicationContext as AC


display :: IORef AC.ContextSettings -> IO ()
display contextSettings = do
  clear [ColorBuffer]
  matrixMode $= Modelview 0
  loadIdentity

  settings <- readIORef contextSettings
  let hScale         = (AC.scaling settings) / (100.0::GLfloat)
      gridOpacity    = (AC.gridOpacity settings) / (100.0::GLfloat)
      surfOpacity    = (AC.surfaceOpacity settings) / (100.0::GLfloat)
      fixedRotation  = (AC.fixedRotation settings) 
      incRotation    = (AC.incRotation settings) 
      accIncRotation = (AC.incRotationAccum settings) 
      maxBeatSamples = (AC.maxBeatBandSamples settings) 
      vscale         = 2.0
      signalLineDist = 0.04::GLfloat
      gridColor      = color3AddAlpha (AC.gridColor settings) gridOpacity
      surfaceColor   = color3AddAlpha (AC.surfaceColor settings) surfOpacity
      lightColor     = color3AddAlpha (AC.lightColor settings) 1
  
  -- Rotate/translate to change view perspective: 
  case AC.renderPerspective settings of
    DT.Isometric -> do
      GL.translate $ Vector3 0 0.1 (-1.7::GLfloat)
      GL.rotate (45::GLfloat) $ Vector3 1.0 0.0 0.0
      GL.rotate (45::GLfloat) $ Vector3 0.0 1.0 0.0
    DT.Top -> do
      GL.translate $ Vector3 0 0 (-1.8::GLfloat)
      GL.rotate (90::GLfloat) $ Vector3 1.0 0.0 0.0
    DT.Front -> do
      GL.translate $ Vector3 0 0 (-2.0::GLfloat)
      GL.rotate (20.0::GLfloat) $ Vector3 1.0 0.0 0.0
    DT.Side -> do
      GL.translate $ Vector3 0 0 (-2.0::GLfloat)
      GL.rotate (20.0::GLfloat) $ Vector3 1.0 0.0 0.0
      GL.rotate (-90::GLfloat) $ Vector3 0.0 1.0 0.0


  GL.diffuse (Light 0) $= lightColor
  GL.diffuse (Light 1) $= lightColor

  
  GL.rotate (DT.rotX fixedRotation) $ Vector3 1.0 0.0 0.0
  GL.rotate (DT.rotX accIncRotation) $ Vector3 1.0 0.0 0.0
  GL.rotate (DT.rotY fixedRotation) $ Vector3 0.0 1.0 0.0
  GL.rotate (DT.rotY accIncRotation) $ Vector3 0.0 1.0 0.0
  GL.rotate (DT.rotZ fixedRotation) $ Vector3 0.0 0.0 1.0
  GL.rotate (DT.rotZ accIncRotation) $ Vector3 0.0 0.0 1.0

  -- Load signal buffer from context
  signalBuf <- readIORef (AC.signalBuf settings)

  let numSignals = length $ DT.signalList signalBuf

  -- Load first signal from buffer to get its length
  let firstSignal = DT.getSignal signalBuf 0
  firstSignalBounds <- getBounds $ DT.signalArray firstSignal
  let numSamples = rangeSize firstSignalBounds

  GL.translate $ Vector3 (-0.5 * vscale) 0 0
  let vLogScale i = log (fromIntegral i * 10.0)
  -- GL.translate $ Vector3 (-0.5 * (sum [ vLogScale x | x <- [0..numSamples] ]) :: GLfloat ) 0 0

  GL.scale 1 hScale (1::GLfloat)
  

  GL.translate $ Vector3 0 0 (-(fromIntegral numSignals) * signalLineDist / 2.0)

  -- [ value_0, ..., value_n ] -> [ Vertex3 index_0 value_0 0, ..., Vertex3 index_n value_n 0 ]
  let toVertexList zVal = zipWith (\i v -> Vertex3 ((fromIntegral i)/(fromIntegral numSamples)*vscale) v (zVal::GLfloat))

  -- Expects a list of samples [GLfloat] and renders them as line:
  let renderSampleLines sampleList = do renderPrimitive LineStrip (
                                          mapM_ GL.vertex (toVertexList 0 [0..numSamples] sampleList) )
                                        GL.translate $ Vector3 0 0 (signalLineDist::GLfloat)

  let tuplesToVertexList idx = zipWith (\i (v1,v2) ->
        let zVal = (fromIntegral idx) * signalLineDist 
            xVal = (fromIntegral i)/(fromIntegral numSamples) 
         -- damp = xVal + (1/xVal * 0.002) in
            damp = log(1000.0 * xVal) / 7.0 in
          [ Vertex3 (xVal*(vLogScale i)) (v1*damp) (zVal::GLfloat),
            Vertex3 (xVal*(vLogScale i)) (v2*damp) (zVal+signalLineDist::GLfloat) ] )
  let renderSampleSurfaceStrip sampleTupleList idx lC bC lbC = do color surfaceColor
                                                                  renderPrimitive TriangleStrip (
                                                                    mapM_ GL.vertex (concat(tuplesToVertexList idx [0..numSamples] sampleTupleList)) )
                                                                  -- color gridColor
                                                                  let beatDamping      = fromIntegral maxBeatSamples
                                                                  let loudnessDamping  = fromIntegral numSamples
                                                                  let localBeatDamping = fromIntegral maxBeatSamples
                                                                  -- let beatGridOpacity  = gridOpacity * ((bC / beatDamping) + (lC / loudnessDamping) + (lbC / localBeatDamping))
                                                                  let beatGridOpacity  = gridOpacity 
                                                                  color $ color3AddAlpha (AC.gridColor settings) beatGridOpacity -- + bC / 100.0)
                                                                  -- translate $ Vector3 0 (0.05::GLfloat) 0
                                                                  renderPrimitive LineStrip (
                                                                    mapM_ GL.vertex (concat(tuplesToVertexList idx [0..numSamples] sampleTupleList)) )
                                                                  -- translate $ Vector3 0 (-0.05::GLfloat) 0

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

  -- Analyze overall loudness: 
  let noisePerSample = 0.2
  let loudness sampleList = (sum sampleList) - (fromIntegral (length sampleList) * noisePerSample)
  let beatLoudness sampleList = (sum $ take maxBeatSamples sampleList) - (fromIntegral maxBeatSamples * noisePerSample)

  -- Render surface as single strips (for z : for x):
  color (Color4 0.7 0.2 0.7 surfOpacity :: Color4 GLfloat)
  let renderSignalSurfaceStrip signals count loudnessCoeff beatCoeff localBeatCoeff = (
        case signals of
          (sig:nextSig:xs) -> (
            do samples <- getElems $ DT.signalArray sig
               nextSamples <- getElems $ DT.signalArray nextSig
               settings <- readIORef contextSettings
               let rangeAmps = AC.rangeAmps settings
               let scaledSamples = RH.bandRangeAmpSamples samples rangeAmps
               let scaledNextSamples = RH.bandRangeAmpSamples nextSamples rangeAmps
               let localBeatCoeff = realToFrac $ (sum $ take maxBeatSamples samples :: GLfloat) - (fromIntegral maxBeatSamples * noisePerSample)
               renderSampleSurfaceStrip (zip scaledSamples scaledNextSamples) count loudnessCoeff beatCoeff localBeatCoeff
               -- recurse
               renderSignalSurfaceStrip(drop 1 signals) (count+1) loudnessCoeff beatCoeff localBeatCoeff
               return () )
          (sig:[]) -> return () )

  let signalList = DT.signalList signalBuf
  let recentSignal = signalList !! (length signalList - 1)
  recentSignalSamples <- getElems $ DT.signalArray recentSignal
  renderSignalSurfaceStrip (signalList) 0 (loudness recentSignalSamples) (beatLoudness recentSignalSamples) 0

  GL.flush

reshape allocation = do
  let rectangleWidthHeight (Gtk.Rectangle _ _ w' h') = (w',h')
  let (w,h) = rectangleWidthHeight allocation
  viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
  perspective (fromIntegral viewPerspective) (fromIntegral w / fromIntegral h) 0.1 100
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
    -- {{{ 
    dither $= Enabled
    normalize $= Enabled -- Automatically normaliye normal vectors to (-1.0,1.0)
    shadeModel $= Smooth
    materialSpecular Front $= Color4 1 1 1 1
    materialShininess Front $= 30
    blend $= Enabled
    polygonSmooth $= Enabled
    lineSmooth $= Enabled
    lighting $= Enabled
    light (Light 0) $= Enabled
    light (Light 1) $= Enabled
    cullFace $= Just Front
    autoNormal $= Enabled
    colorMaterial $= Just (Front, AmbientAndDiffuse)
    blendFunc $= (SrcAlpha, One)
    -- blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
    -- blendFunc $= (SrcAlphaSaturate, One)
    -- blendFunc $= (OneMinusSrcColor, One)
    -- depthFunc $= Just Less
    hint PerspectiveCorrection $= Nicest
    hint PolygonSmooth $= Nicest
    hint LineSmooth $= Nicest

    matrixMode $= Projection
    loadIdentity
    viewport $= (Position 0 0, Size (fromIntegral canvasInitWidth) (fromIntegral canvasInitHeight))
    perspective (fromIntegral viewPerspective) (fromIntegral canvasInitWidth / fromIntegral canvasInitHeight) 0.1 100
    matrixMode $= Modelview 0

    loadIdentity

    GL.position (Light 0) $= (Vertex4 (-1.0) 3.0 (-2.0) 0.0)
    GL.diffuse (Light 0) $= Color4 0.6 0.6 0.6 1.0
    GL.position (Light 0) $= (Vertex4 (1.0) 3.0 (2.0) 0.0)
    GL.diffuse (Light 1) $= Color4 0.4 0.4 1.0 1.0

    return ()
    -- }}}

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
  Gtk.widgetSetSizeRequest canvas canvasInitWidth canvasInitHeight

  Gtk.set window [ Gtk.containerChild := canvas ]

  settings <- readIORef contextSettings
  let timeoutMs = (Conv.freqToMs $ AC.renderingFrequency settings)
  -- Redraw canvas according to rendering frequency:
  updateCanvasTimer <- Gtk.timeoutAddFull (do
      Gtk.widgetQueueDraw canvas
      return True)
    Gtk.priorityDefaultIdle timeoutMs

  -- Remove timer for redrawing canvas when closing window:
  _ <- Gtk.onDestroy window (Gtk.timeoutRemove updateCanvasTimer)

  Gtk.widgetShowAll window


color3AddAlpha :: Color3 GLfloat -> GLfloat -> Color4 GLfloat
color3AddAlpha (Color3 r g b) a = Color4 r g b a

canvasInitWidth :: Int
canvasInitWidth = 800
canvasInitHeight :: Int
canvasInitHeight = 800

viewPerspective :: Int
viewPerspective = 90
