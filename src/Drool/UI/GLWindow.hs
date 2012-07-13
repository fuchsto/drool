-----------------------------------------------------------------------------
--
-- Module      :  Drool.UI.GLWindow
-- Copyright   :  Tobias Fuchs
-- License     :  AllRightsReserved
--
-- Maintainer  :  twh.fuchs@gmail.com
-- Stability   :  experimental
-- Portability :  POSIX
--
-- |
--
-----------------------------------------------------------------------------

{-# OPTIONS -O2 -Wall #-}

module Drool.UI.GLWindow (
    initComponent
) where

-- import Debug.Trace

import Data.IORef(IORef, readIORef, newIORef, modifyIORef, writeIORef )
import Data.Array.IO

import Graphics.Rendering.OpenGL as GL

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))

import qualified Graphics.UI.Gtk.OpenGL as GtkGL

import qualified Drool.Utils.Conversions as Conv
import qualified Drool.Utils.Objects as Obj
import qualified Drool.Utils.RenderHelpers as RH
import qualified Drool.Utils.FeatureExtraction as FE ( SignalFeatures(..), SignalFeaturesList(..) )
import qualified Drool.Types as DT
import qualified Drool.ApplicationContext as AC

display :: IORef AC.ContextSettings -> IORef RH.RenderSettings -> IO ()
display contextSettingsIORef renderSettingsIORef = do
-- {{{
  renderSettings <- readIORef renderSettingsIORef
  settings       <- readIORef contextSettingsIORef

  let blendModeSource      = Conv.blendModeSourceFromIndex $ AC.blendModeSourceIdx settings
  let blendModeFrameBuffer = Conv.blendModeFrameBufferFromIndex $ AC.blendModeFrameBufferIdx settings
	
  blendFunc $= (blendModeSource, blendModeFrameBuffer)
  clear [ColorBuffer, DepthBuffer]
  
  matrixMode $= Modelview 0
  loadIdentity

  let hScale         = (AC.scaling settings) / (100.0::GLfloat)
      gridOpacity    = (AC.gridOpacity settings) / (100.0::GLfloat)
      surfOpacity    = (AC.surfaceOpacity settings) / (100.0::GLfloat)
      fixedRotation  = (AC.fixedRotation settings) 
      incRotation    = (AC.incRotation settings) 
      accIncRotation = (AC.incRotationAccum settings) 
      maxNumSignals  = (AC.signalBufferSize settings)
      maxBeatSamples = (AC.maxBeatBand settings) 
      lightPos0      = (RH.lightPos0 renderSettings)
      lightPos1      = (RH.lightPos1 renderSettings)
      gridColor      = RH.color3AddAlpha (AC.gridColor settings) gridOpacity
      surfaceColor   = RH.color3AddAlpha (AC.surfaceColor settings) surfOpacity
      lightColor     = RH.color3AddAlpha (AC.lightColor settings) 1
  
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
  GL.specular (Light 0) $= mulColor4Value lightColor 1.5
  GL.specular (Light 1) $= mulColor4Value lightColor 1.5
  
  GL.rotate (DT.rotX fixedRotation) $ Vector3 1.0 0.0 0.0
  GL.rotate (DT.rotX accIncRotation) $ Vector3 1.0 0.0 0.0
  GL.rotate (DT.rotY fixedRotation) $ Vector3 0.0 1.0 0.0
  GL.rotate (DT.rotY accIncRotation) $ Vector3 0.0 1.0 0.0
  GL.rotate (DT.rotZ fixedRotation) $ Vector3 0.0 0.0 1.0
  GL.rotate (DT.rotZ accIncRotation) $ Vector3 0.0 0.0 1.0

  ---------------------------------------------------------------------------------------------------
  -- End of perspective transformations
  ---------------------------------------------------------------------------------------------------

  -- Load signal buffer from context
  signalBuf   <- readIORef $ RH.signalBuf renderSettings
  featuresBuf <- readIORef $ RH.featuresBuf renderSettings
  
  let signalList = DT.signalList signalBuf
  let numNewSignals = length signalList
  -- Load vertex buffer from rendering context
  let vertexBufIORef = RH.vertexBuf renderSettings
  -- Load normals buffer from rendering context
  let normalsBufIORef = RH.normalsBuf renderSettings
  -- Load features buffer from rendering context
  let featuresBufIORef = RH.featuresBuf renderSettings

  ---------------------------------------------------------------------------------------------------
  -- Begin handling of new signal
  ---------------------------------------------------------------------------------------------------

  let xPosFun   = RH.xPosFun renderSettings
  let zPosFun   = RH.zPosFun renderSettings
  let rangeAmps = AC.rangeAmps settings

  let newSignals = DT.signalList signalBuf
  -- All signals in buffer loaded, empty signal buffer: 
  writeIORef (RH.signalBuf renderSettings) (DT.CSignalList []) 

  newSigVertices <- mapM ( \sig -> do sigSamples <- getElems $ DT.signalArray sig
                                      let sigSamplesT = RH.bandRangeAmpSamples (RH.scaleSamples sigSamples hScale) rangeAmps
                                      let sigVertices = RH.verticesFromSamples sigSamplesT 0 renderSettings
                                      return sigVertices ) newSignals
  vBufCurr <- readIORef vertexBufIORef
  let vBufUpdated   = newSigVertices ++ (Conv.adjustBufferSizeBack vBufCurr (maxNumSignals-(length newSigVertices)))
  let vBufZAdjusted = RH.updateVerticesZCoord vBufUpdated zPosFun 
  modifyIORef vertexBufIORef ( \_ -> vBufZAdjusted )
  
  nBufCurr <- readIORef normalsBufIORef
  let nBufUpdated = updateNormalsBuffer nBufCurr (take ((length newSigVertices)+2) vBufZAdjusted) maxNumSignals
  modifyIORef normalsBufIORef ( \_ -> nBufUpdated )
  
  -- Load most recent signal from buffer (last signal in list): 
  let recentSignal = DT.getRecentSignal signalBuf 
  -- Get length of most recent signal (= number of samples per signal): 
  signalBounds <- getBounds $ DT.signalArray recentSignal
  let numSamples = rangeSize signalBounds
  -- Transform samples in recent signal: 
  recentSignalSamples <- getElems $ DT.signalArray recentSignal

  ---------------------------------------------------------------------------------------------------
  -- End handling of new signal
  ---------------------------------------------------------------------------------------------------
  
  let surfaceWidth = xPosFun (numSamples-1)
  let surfaceDepth = zPosFun (maxNumSignals-1)

  GL.position (Light 0) $= lightPos0
  GL.position (Light 0) $= lightPos1

  GL.translate $ Vector3 (-0.5 * surfaceWidth) 0 0
  GL.translate $ Vector3 0 0 (-0.5 * surfaceDepth)
  
  materialSpecular FrontAndBack $= mulColor4Value surfaceColor 2
  materialShininess FrontAndBack $= 30

  vertexBuf  <- readIORef vertexBufIORef :: IO [[ Vertex3 GLfloat ]]
  normalsBuf <- readIORef normalsBufIORef :: IO [[ Normal3 GLfloat ]]

  ----------------------------------------------------------------------------------------
  -- Render scene 
  ----------------------------------------------------------------------------------------
  -- Get inverse of model view matrix: 
  glModelViewMatrix <- get currentMatrix :: IO (GLmatrix GLfloat)
	-- Resolve view point in model view coordinates: 
  viewpoint <- RH.getViewpointFromModelView glModelViewMatrix
  
  RH.renderSurface vertexBuf normalsBuf (FE.signalFeaturesList featuresBuf) viewpoint numSamples settings renderSettings
-- }}} 

reshape :: Gtk.Rectangle -> IO ()
reshape allocation = do
  let rectangleWidthHeight (Gtk.Rectangle _ _ w' h') = (w',h')
  let (w,h) = rectangleWidthHeight allocation
  viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
  perspective (fromIntegral viewPerspective) (fromIntegral w / fromIntegral h) 0.1 100
  matrixMode $= Modelview 0
  return ()

-- Expects current normals buffer, list of signals as vertices and maximum number of signals in buffers. 
-- For every signal in the second argument, a list of normal vectors is appended to the given 
-- normals buffer. 
-- Returns updated normals buffer. 
updateNormalsBuffer :: [[ Normal3 GLfloat ]] -> [[ Vertex3 GLfloat ]] -> Int -> [[ Normal3 GLfloat ]] 
updateNormalsBuffer nBuf vBuf@(_:_:_:vBufTail) numSignals = updateNormalsBuffer updatedNormalsBuf (tail vBuf) numSignals
-- {{{
  where 
  -- Drop first and last element of nBuf. 
  -- Last element has incomplete normals and will be replaced by element 
  -- with correct normals. This happens in any case. 
  -- Drop first element if max buffer size is exceeded. 
    adjNormalsBuf = take (numSignals-2) (Conv.adjustBufferSize nBuf (numSignals-1))
    vBufLen = length vBuf
  
  -- Take 3 most recent signals from vertex buffer. 
  -- Last element in signal buffer is most recent signal: 
    sigCurrIdx  = 0
    sigPrev1Idx = if length vBuf > 1 then 1 else sigCurrIdx
    sigPrev2Idx = if length vBuf > 2 then 2 else sigPrev1Idx
    
    sigCurr  = if vBufLen > 0 then (vBuf !! sigCurrIdx)  else [Vertex3 0.0 0.0 0.0] :: [Vertex3 GLfloat]
    sigPrev1 = if vBufLen > 0 then (vBuf !! sigPrev1Idx) else [Vertex3 0.0 0.0 0.0] :: [Vertex3 GLfloat]
    sigPrev2 = if vBufLen > 0 then (vBuf !! sigPrev2Idx) else [Vertex3 0.0 0.0 0.0] :: [Vertex3 GLfloat]
  
  -- Normal vector for current signal is incomplete: 
    verticesAndNormalsCurr = RH.normalsFromVertices [sigPrev1,sigCurr,[]] 
  -- Normal vector for prev signal is complete: 
    verticesAndNormalsPrev = RH.normalsFromVertices [sigPrev2,sigPrev1,sigCurr] 

    updatedNormalsBuf = verticesAndNormalsCurr : verticesAndNormalsPrev : adjNormalsBuf 
-- }}}

updateNormalsBuffer nBuf _ _ = nBuf

-- Component init interface for main UI. 
initComponent _ contextSettings contextObjects = do
-- {{{
  window <- Gtk.windowNew

  Gtk.set window [ Gtk.containerBorderWidth := 0,
                   Gtk.windowTitle := "drool visualizer" ]

  putStrLn "Initializing OpenGL viewport"

  glConfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA, GtkGL.GLModeMultiSample,
                                 GtkGL.GLModeDouble, GtkGL.GLModeDepth, GtkGL.GLModeAlpha]
  _ <- GtkGL.initGL
  
  canvas <- GtkGL.glDrawingAreaNew glConfig
  
  settings <- readIORef contextSettings
  objects  <- readIORef contextObjects
  
  vertexBufIORef  <- newIORef []
  normalsBufIORef <- newIORef []

  renderSettings <- newIORef RH.RenderSettings { RH.signalBuf = AC.signalBuf objects, 
                                                 RH.featuresBuf = AC.featuresBuf objects, 
                                                 RH.xPosFun = (\x -> fromIntegral x / 60.0), 
                                                 RH.zPosFun = (\z -> fromIntegral z / 15.0), 
                                                 RH.scaleFun = (\s _ _ -> s), 
                                                 RH.vertexBuf = vertexBufIORef, 
                                                 RH.normalsBuf = normalsBufIORef, 
                                                 RH.lightPos0 = (Vertex4 (-1.0) 3.0 (-2.0) 0.0), 
                                                 RH.lightPos1 = (Vertex4 (1.0) 3.0 (2.0) 0.0), 
                                                 RH.numSignals = 0, 
                                                 RH.numSamples = 0 } 

  -- Initialise some GL setting just before the canvas first gets shown
  -- (We can't initialise these things earlier since the GL resources that
  -- we are using wouldn't have been setup yet)

  _ <- Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ -> do
    -- {{{ 
    cSettings <- readIORef contextSettings
    -- depthMask $= Disabled
    -- dither $= Enabled
    normalize $= Enabled -- Automatically normaliye normal vectors to (-1.0,1.0)
    shadeModel $= Smooth
    depthFunc $= Just Less
    polygonSmooth $= Enabled
    lineSmooth $= Enabled
    lighting $= Enabled
    light (Light 0) $= Enabled
    light (Light 1) $= Enabled
    frontFace $= CCW
    blend $= Enabled
    multisample $= Enabled
    sampleAlphaToCoverage $= Enabled
    
    lineWidthRange <- get smoothLineWidthRange
    lineWidth $= fst lineWidthRange -- use thinnest possible lines

    cullFace $= Just Back
    colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)

    let blendModeSource = Conv.blendModeSourceFromIndex $ AC.blendModeSourceIdx settings
    let blendModeFrameBuffer = Conv.blendModeFrameBufferFromIndex $ AC.blendModeFrameBufferIdx settings
    blendFunc $= (blendModeSource, blendModeFrameBuffer)
    
    hint PerspectiveCorrection $= Nicest
    hint PolygonSmooth $= Nicest
    hint LineSmooth $= Nicest

    matrixMode $= Projection
    loadIdentity
    viewport $= (Position 0 0, Size (fromIntegral canvasInitWidth) (fromIntegral canvasInitHeight))
    perspective (fromIntegral viewPerspective) (fromIntegral canvasInitWidth / fromIntegral canvasInitHeight) 0.1 10

    matrixMode $= Modelview 0
    loadIdentity
  
    return ()
    -- }}}

  -- OnShow handler for GL canvas:
  _ <- Gtk.onExpose canvas $ \_ -> do
    GtkGL.withGLDrawingArea canvas $ \glwindow -> do
      GL.clear [GL.DepthBuffer, GL.ColorBuffer]
      display contextSettings renderSettings
      GtkGL.glDrawableSwapBuffers glwindow
    return True

  -- Resize handler:
  _ <- Gtk.onSizeAllocate canvas (reshape)

  -- Add canvas (OpenGL drawing area) to GUI:
  Gtk.widgetSetSizeRequest canvas canvasInitWidth canvasInitHeight

  Gtk.set window [ Gtk.containerChild := canvas ]

  let timeoutMs = (Conv.freqToMs $ AC.renderingFrequency settings)
  -- Redraw canvas according to rendering frequency:
  updateCanvasTimer <- Gtk.timeoutAddFull (do
      Gtk.widgetQueueDraw canvas
      return True)
    Gtk.priorityDefaultIdle timeoutMs

  -- Remove timer for redrawing canvas when closing window:
  _ <- Gtk.onDestroy window (Gtk.timeoutRemove updateCanvasTimer)

  Gtk.widgetShowAll window
-- }}} 

canvasInitWidth :: Int
canvasInitWidth = 800
canvasInitHeight :: Int
canvasInitHeight = 800

mulColor4Value :: Color4 GLfloat -> GLfloat -> Color4 GLfloat
mulColor4Value (Color4 r g b a) value = Color4 r' g' b' a'
  where r' = r * value
        g' = g * value
        b' = b * value
        a' = a * value

viewPerspective :: Int
viewPerspective = 90
