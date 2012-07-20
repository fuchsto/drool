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

import Debug.Trace

import Data.IORef(IORef, readIORef, newIORef, modifyIORef, writeIORef )
import Data.Array.IO

import Control.Monad.Trans ( liftIO )

import qualified Graphics.UI.Gtk.Builder as GtkBuilder
import Graphics.Rendering.OpenGL as GL 
import qualified Graphics.Rendering.FTGL as FTGL

import Graphics.UI.Gtk as Gtk

import qualified Graphics.UI.Gtk.OpenGL as GtkGL

import qualified Drool.Utils.SigGen as SigGen ( SignalGenerator(..) )
import qualified Drool.Utils.Conversions as Conv
import qualified Drool.Utils.RenderHelpers as RH
import qualified Drool.Utils.FeatureExtraction as FE ( SignalFeaturesList(..) )
import qualified Drool.Types as DT
import qualified Drool.ApplicationContext as AC

import qualified Control.Concurrent.MVar as MV ( MVar, swapMVar )

import Criterion.Main
import Criterion.Config


display :: IORef AC.ContextSettings -> IORef RH.RenderSettings -> IO ()
display contextSettingsIORef renderSettingsIORef = do
-- {{{
  renderSettingsOld <- readIORef renderSettingsIORef
  settings          <- readIORef contextSettingsIORef

  let samplingSem = RH.samplingSem renderSettingsOld
  numNewSignals <- MV.swapMVar samplingSem 0

  matrixMode $= Projection
  loadIdentity
  perspective (realToFrac (AC.viewAngle settings)) (fromIntegral canvasInitWidth / fromIntegral canvasInitHeight) 0.1 10

  clear [ColorBuffer, DepthBuffer]
  
  matrixMode $= Modelview 0
  loadIdentity

  let hScale         = (AC.scaling settings) / (100.0::GLfloat)
      surfOpacity    = (AC.surfaceOpacity settings) / (100.0::GLfloat)
      fixedRotation  = AC.fixedRotation settings
      accIncRotation = AC.incRotationAccum settings
      viewDistance   = AC.viewDistance settings
      maxNumSignals  = AC.signalBufferSize settings
      lightPos0      = RH.lightPos0 renderSettingsOld
      lightPos1      = RH.lightPos1 renderSettingsOld
      sigGen         = RH.signalGenerator renderSettingsOld
      surfaceColor   = RH.color3AddAlpha (AC.surfaceColor settings) surfOpacity
      lightColor     = RH.color3AddAlpha (AC.lightColor settings) 1
      vPerspective   = AC.renderPerspective settings

  let tick   = RH.tick renderSettingsOld
  let tickMs = tick * 25

  let updatePerspective p = if AC.autoPerspectiveSwitch settings && tickMs >= AC.autoPerspectiveSwitchInterval settings then ( do 
                                let nextPerspective = RH.nextPerspective p
                                modifyIORef contextSettingsIORef ( \_ -> settings { AC.renderPerspective = nextPerspective } )
                                modifyIORef renderSettingsIORef ( \_ -> renderSettingsOld { RH.tick = 0 } )
                                return nextPerspective )
                            else return p
  curPerspective <- updatePerspective vPerspective


  let blendModeSource      = Conv.blendModeSourceFromIndex $ AC.blendModeSourceIdx settings
  let blendModeFrameBuffer = Conv.blendModeFrameBufferFromIndex $ AC.blendModeFrameBufferIdx settings
  
  blendFunc $= (blendModeSource, blendModeFrameBuffer)
  
  GL.translate $ Vector3 0 0 viewDistance

  -- Rotate/translate to change view perspective: 
  case curPerspective of
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
  signalBuf   <- readIORef $ RH.signalBuf renderSettingsOld
  -- Load features buffer from rendering context
  featuresBuf <- readIORef $ RH.featuresBuf renderSettingsOld
  
  -- Load vertex buffer from rendering context
  let vertexBufIORef = RH.vertexBuf renderSettingsOld
  -- Load normals buffer from rendering context
  let normalsBufIORef = RH.normalsBuf renderSettingsOld

  ---------------------------------------------------------------------------------------------------
  -- Begin handling of new signal
  ---------------------------------------------------------------------------------------------------

  let newSignals = take numNewSignals $ DT.signalList signalBuf
  let numSignals = length $ DT.signalList signalBuf
  let numNewSignalsRead = length newSignals
  -- Load most recent signal from buffer (last signal in list): 
  let recentSignal = DT.getRecentSignal signalBuf 
  -- Get length of most recent signal (= number of samples per signal): 
  numSamples <- case recentSignal of 
                     Just s  -> do signalBounds <- getBounds $ DT.signalArray s
                                   return $ rangeSize signalBounds
                     Nothing -> return $ SigGen.numSamples sigGen

  modifyIORef renderSettingsIORef ( \_ -> renderSettingsOld { RH.tick = (tick+1) `mod` 10000, 
                                                              RH.xLinScale = AC.xLinScale settings, 
                                                              RH.xLogScale = AC.xLogScale settings, 
                                                              RH.zLinScale = AC.zLinScale settings, 
                                                              RH.numSignals = numSignals, 
                                                              RH.numSamples = numSamples } )
  renderSettings <- readIORef renderSettingsIORef

  let xPosFun   = RH.xPosFun renderSettings
  let zPosFun   = RH.zPosFun renderSettings
  let rangeAmps = AC.rangeAmps settings

  -- All signals in buffer loaded, empty signal buffer: 
  -- writeIORef (RH.signalBuf renderSettings) (DT.CSignalList []) 

  newSigVertices <- mapM ( \sig -> do sigSamples <- getElems $ DT.signalArray sig
                                      let sigSamplesT = RH.bandRangeAmpSamples (RH.scaleSamples sigSamples hScale) rangeAmps
                                      let sigVertices = RH.verticesFromSamples sigSamplesT 0 renderSettings
                                      return sigVertices ) (reverse newSignals)

  vBufCurr <- readIORef vertexBufIORef
  let vBufUpdated     = if numNewSignalsRead > 0 then newSigVertices ++ (Conv.adjustBufferSizeBack vBufCurr (numSignals-numNewSignalsRead)) else vBufCurr
  let vBufZAdjusted   = if numNewSignalsRead > 0 then RH.updateVerticesZCoord vBufUpdated zPosFun renderSettings else vBufUpdated
  modifyIORef vertexBufIORef ( \_ -> vBufZAdjusted )

  nBufCurr <- readIORef normalsBufIORef
  let nBufUpdated = if numNewSignalsRead > 0 then updateNormalsBuffer nBufCurr (take (numNewSignalsRead+2) vBufZAdjusted) numSignals else nBufCurr
  modifyIORef normalsBufIORef ( \_ -> nBufUpdated )
  
  ---------------------------------------------------------------------------------------------------
  -- End handling of new signal
  ---------------------------------------------------------------------------------------------------
  
  let surfaceWidth = xPosFun (numSamples-1) renderSettings
  let surfaceDepth = zPosFun (numSignals-1) renderSettings

  fogMode $= Linear 0.0 (surfaceDepth * 2.0)
  fogColor $= (Color4 0.0 0.0 0.0 1.0)

  GL.position (Light 0) $= lightPos0
  GL.position (Light 0) $= lightPos1

  GL.translate $ Vector3 (-0.5 * surfaceWidth) 0 0
  GL.translate $ Vector3 0 0 (-0.5 * surfaceDepth)
  
  materialSpecular FrontAndBack $= mulColor4Value surfaceColor 2
  materialShininess FrontAndBack $= 30

  -- vertexBuf  <- readIORef vertexBufIORef :: IO [[ Vertex3 GLfloat ]]
  -- normalsBuf <- readIORef normalsBufIORef :: IO [[ Normal3 GLfloat ]]
  let vertexBuf  = vBufZAdjusted
  let normalsBuf = nBufUpdated

  ----------------------------------------------------------------------------------------
  -- Render scene 
  ----------------------------------------------------------------------------------------
  -- Get inverse of model view matrix: 
  glModelViewMatrix <- GL.get (matrix (Just (Modelview 0))) :: IO (GLmatrix GLfloat)
  -- Resolve view point in model view coordinates: 
  viewpoint <- RH.getViewpointFromModelView glModelViewMatrix
  
  b <- bench "renderSurface" $ (RH.renderSurface vertexBuf normalsBuf (FE.signalFeaturesList featuresBuf) viewpoint numSamples settings renderSettings)

  -- Render marquee text, if any
  blendFunc $= ( One, One )
  colorMaterial $= Just (Front, AmbientAndDiffuse)
  materialAmbient Front $= mulColor4Value surfaceColor 2
  materialSpecular Front $= mulColor4Value surfaceColor 4
  materialShininess Front $= 30
  let marqueeText = AC.marqueeText settings
  gridfont <- readIORef $ RH.gridFont renderSettings
  fillfont <- readIORef $ RH.fillFont renderSettings

  marqueeBBox <- FTGL.getFontBBox gridfont marqueeText 

  preservingMatrix $ do 
    fog $= Disabled
    let fontWidth  = realToFrac $ (marqueeBBox !! 3) - (marqueeBBox !! 0)
    -- let fontHeight = realToFrac $ (marqueeBBox !! 4) - (marqueeBBox !! 1)
    let fontScaling = (surfaceWidth * 1.5) / fontWidth
    GL.scale fontScaling fontScaling (0 :: GLfloat)
    translate $ Vector3 (-0.5 * fontWidth + 0.5 * surfaceWidth / fontScaling) (0.7 / fontScaling) (-4.5 :: GLfloat)
    color $ mulColor4Value surfaceColor 4
    FTGL.renderFont fillfont marqueeText FTGL.Front
    blendFunc $= ( SrcAlpha, OneMinusSrcAlpha )
    color $ Color4 0 0 0 (0.5::GLfloat)
    FTGL.renderFont gridfont marqueeText FTGL.Front
    fog $= Enabled

-- }}} 

reshape :: IORef AC.ContextSettings -> Gtk.Rectangle -> IO ()
reshape settingsIORef allocation = do
  let rectangleWidthHeight (Gtk.Rectangle _ _ w' h') = (w',h')
  let (w,h) = rectangleWidthHeight allocation
  settings <- readIORef settingsIORef
  let viewAngle = AC.viewAngle settings
  matrixMode $= Projection
  viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
  perspective (realToFrac viewAngle) (fromIntegral w / fromIntegral h) 0.1 100
  matrixMode $= Modelview 0
  return ()

-- Expects current normals buffer, list of signals as vertices and maximum number of signals in buffers. 
-- For every signal in the second argument, a list of normal vectors is appended to the given 
-- normals buffer. 
-- Returns updated normals buffer. 
updateNormalsBuffer :: [[ Normal3 GLfloat ]] -> [[ Vertex3 GLfloat ]] -> Int -> [[ Normal3 GLfloat ]] 
-- {{{
updateNormalsBuffer nBuf vBuf@(_:_:_:_) numSignals = case vBuf of 
  (_:_:_:_) -> updateNormalsBuffer updatedNormalsBuf (tail vBuf) numSignals
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
  _ -> []
-- }}}

updateNormalsBuffer nBuf _ _ = nBuf

-- Component init interface for main UI. 
initComponent :: GtkBuilder.Builder -> IORef AC.ContextSettings -> IORef AC.ContextObjects -> IO ()
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

  gridfont <- FTGL.createOutlineFont "ProggyClean.ttf"
  fillfont <- FTGL.createPolygonFont "ProggyClean.ttf"
  _ <- FTGL.setFontFaceSize gridfont 36 36 
  _ <- FTGL.setFontFaceSize fillfont 36 36 

  gridFontIORef <- newIORef gridfont
  fillFontIORef <- newIORef fillfont

  let sigGen     = AC.signalGenerator objects
  let numSamples = SigGen.numSamples sigGen

  let xPosFun x rs = (log (x'+1.0) + (x' / n' * xLogScale)) / (log n' + xLogScale) * xLinScale
                     where xLogScale = RH.xLogScale rs
                           xLinScale = RH.xLinScale rs
                           sGen = RH.signalGenerator rs
                           n' = fromIntegral $ SigGen.numSamples sGen
                           x' = fromIntegral x

  -- Returns Infinity for numSignals = 0
  let zPosFun z rs = fromIntegral z / n' * zLinScale
                     where zLinScale = RH.zLinScale rs
                           n' = fromIntegral $ RH.numSignals rs
  
  renderSettings <- newIORef RH.RenderSettings { RH.signalGenerator = sigGen, 
                                                 RH.samplingSem = AC.samplingSem objects, 
                                                 RH.signalBuf = AC.signalBuf objects, 
                                                 RH.featuresBuf = AC.featuresBuf objects, 
                                                 RH.xPosFun = xPosFun, 
                                                 RH.zPosFun = zPosFun, 
                                                 RH.xLinScale = AC.xLinScale settings, 
                                                 RH.xLogScale = AC.xLogScale settings, 
                                                 RH.zLinScale = AC.zLinScale settings,
                                                 RH.scaleFun = (\s _ _ -> s), 
                                                 RH.vertexBuf = vertexBufIORef, 
                                                 RH.normalsBuf = normalsBufIORef, 
                                                 RH.lightPos0 = (Vertex4 (-1.0) 3.0 (-2.0) 0.0), 
                                                 RH.lightPos1 = (Vertex4 (1.0) 3.0 (2.0) 0.0), 
                                                 RH.fillFont = fillFontIORef,
                                                 RH.gridFont = gridFontIORef, 
                                                 RH.tick = 0, 
                                                 RH.numSignals = 0, 
                                                 RH.numSamples = 0 } 

  -- Initialise some GL setting just before the canvas first gets shown
  -- (We can't initialise these things earlier since the GL resources that
  -- we are using wouldn't have been setup yet)

  _ <- Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ -> do
    -- {{{ 
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
    fog $= Enabled

    lineWidthRange <- GL.get smoothLineWidthRange
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
    perspective (realToFrac $ AC.viewAngle settings) (fromIntegral canvasInitWidth / fromIntegral canvasInitHeight) 0.1 10

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
  _ <- Gtk.onSizeAllocate canvas (reshape contextSettings)

  -- Add canvas (OpenGL drawing area) to GUI:
  Gtk.widgetSetSizeRequest canvas canvasInitWidth canvasInitHeight

  Gtk.set window [ Gtk.containerChild := canvas ]

  -- Fullscreen mode: 
  _ <- Gtk.on window Gtk.keyPressEvent $ Gtk.tryEvent $ do 
    [Gtk.Control] <- Gtk.eventModifier
    "f" <- Gtk.eventKeyName
    liftIO $ Gtk.windowSetKeepAbove window True
    liftIO $ Gtk.windowFullscreen window
  
  _ <- Gtk.on window Gtk.keyPressEvent $ Gtk.tryEvent $ do 
    "Escape" <- Gtk.eventKeyName
    liftIO $ Gtk.windowUnfullscreen window
    liftIO $ Gtk.windowSetKeepAbove window False

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
canvasInitHeight = 600

mulColor4Value :: Color4 GLfloat -> GLfloat -> Color4 GLfloat
mulColor4Value (Color4 r g b a) value = Color4 r' g' b' a'
  where r' = r * value
        g' = g * value
        b' = b * value
        a' = a * value

