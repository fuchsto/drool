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

import Debug.Trace

import Data.IORef(IORef, readIORef, newIORef, modifyIORef, writeIORef )
import Data.Array.IO

import Graphics.Rendering.OpenGL as GL

import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk (AttrOp((:=)))

import qualified Graphics.UI.Gtk.OpenGL as GtkGL

import qualified Drool.Utils.Conversions as Conv
import qualified Drool.Utils.Objects as Obj
import qualified Drool.Utils.RenderHelpers as RH
import qualified Drool.Types as DT
import qualified Drool.ApplicationContext as AC


display :: IORef AC.ContextSettings -> IORef RH.RenderSettings -> IO ()
display contextSettingsIORef renderSettingsIORef = do
  renderSettings <- readIORef renderSettingsIORef
  settings <- readIORef contextSettingsIORef

  let blendModeSource = AC.blendModeSource settings
  let blendModeFrameBuffer = AC.blendModeFrameBuffer settings
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
      maxBeatSamples = (AC.maxBeatBandSamples settings) 
      lightPos0      = (RH.lightPos0 renderSettings)
      lightPos1      = (RH.lightPos1 renderSettings)
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
  signalBuf <- readIORef (AC.signalBuf settings)
  let signalList = DT.signalList signalBuf
  let numSignals = length signalList
  -- Load vertex buffer from rendering context
  let vertexBufIORef = RH.vertexBuf renderSettings
  -- Load normals buffer from rendering context
  let normalsBufIORef = RH.normalsBuf renderSettings

  ---------------------------------------------------------------------------------------------------
  -- Begin handling of new signal
  ---------------------------------------------------------------------------------------------------

  let xPosFun = RH.xPosFun renderSettings
  let zPosFun = RH.zPosFun renderSettings

  -- Load most recent signal from buffer (last signal in list): 
  let recentSignal = DT.getRecentSignal signalBuf 
  -- Get length of most recent signal (= number of samples per signal): 
  signalBounds <- getBounds $ DT.signalArray recentSignal
  let numSamples = rangeSize signalBounds
  -- Transform samples in recent signal: 
  recentSignalSamples <- getElems $ DT.signalArray recentSignal
  let rangeAmps = AC.rangeAmps settings
  let recentSignalSamplesTrans = RH.bandRangeAmpSamples (RH.scaleSamples recentSignalSamples hScale) rangeAmps
  -- Convert transformed samples of recent signal to vertices. 
  -- Use 0 for all z values: 
  let recentSignalVertices = RH.verticesFromSamples recentSignalSamplesTrans 0 renderSettings
  -- Push vertices of recent signal to vertex buffer. 
  -- Also updates z values of all vertices in vertex buffer: 
  vBufCurr <- readIORef vertexBufIORef
  let vertexBufferPushedSignal = recentSignalVertices : (Conv.adjustBufferSizeBack ( vBufCurr) (numSignals-1))
  let verticesWithAdjCoords = RH.updateVerticesZCoord vertexBufferPushedSignal zPosFun 
  modifyIORef vertexBufIORef ( \_ -> verticesWithAdjCoords )
  -- Now that all new vertices are ready, update the normals buffer: 
  nBufCurr <- readIORef normalsBufIORef
  updatedNormalsBuf <- updateNormalsBuffer nBufCurr verticesWithAdjCoords numSignals numSamples
  modifyIORef normalsBufIORef ( \_ -> updatedNormalsBuf )
  -- Update numSignals and numSamples: 
  modifyIORef renderSettingsIORef (\rs -> renderSettings { RH.numSignals = numSignals, RH.numSamples = numSamples } ) 
  
  ---------------------------------------------------------------------------------------------------
  -- End handling of new signal
  ---------------------------------------------------------------------------------------------------
  
  -- let surfaceWidth = (fromIntegral numSamples) * (xPosFun 1) :: GLfloat
  -- let surfaceDepth = (fromIntegral numSignals) * (zPosFun 1) :: GLfloat
  let surfaceWidth = xPosFun (numSamples-1)
  let surfaceDepth = zPosFun (numSignals-1)

  GL.position (Light 0) $= lightPos0
  GL.position (Light 0) $= lightPos1

  preservingMatrix $ do 
     let cubePos = RH.vertexToVector $ RH.vx4ToVx3 lightPos0
     translate cubePos
     Obj.renderCube 0.1
     Obj.renderCubeFrame 0.11
  preservingMatrix $ do 
     let cubePos = RH.vertexToVector $ RH.vx4ToVx3 lightPos1
     translate cubePos
     Obj.renderCube 0.1
     Obj.renderCubeFrame 0.11

  GL.translate $ Vector3 (-0.5 * surfaceWidth) 0 0
  GL.translate $ Vector3 0 0 (-0.75 * surfaceDepth)
  
  materialSpecular FrontAndBack $= mulColor4Value surfaceColor 1.5
  materialShininess FrontAndBack $= 30

  let renderSampleSurfaceStrip vertices normals idx lC bC lbC = do color surfaceColor
                                                                   renderPrimitive TriangleStrip (
                                                                     if AC.useNormals settings then
                                                                       mapM_ RH.vertexWithNormal ( zip vertices normals ) 
                                                                     else 
                                                                       mapM_ vertex vertices )
                                                                   let beatDamping      = fromIntegral maxBeatSamples
                                                                   let loudnessDamping  = fromIntegral numSamples
                                                                   let localBeatDamping = fromIntegral maxBeatSamples
                                                                   let beatGridOpacity  = gridOpacity * ((bC / beatDamping) + (lC / loudnessDamping) + (lbC / localBeatDamping))
                                                                   -- mapM_ RH.drawNormal (zip vertices normals)
                                                                   color $ color3AddAlpha (AC.gridColor settings) beatGridOpacity -- + bC / 100.0)
                                                                   translate $ Vector3 0 (0.02::GLfloat) 0
                                                                   renderPrimitive LineStrip ( 
                                                                     if AC.useNormals settings then
                                                                       mapM_ RH.vertexWithNormal ( zip vertices normals ) 
                                                                     else 
                                                                       mapM_ vertex vertices )
                                                                   translate $ Vector3 0 (-0.02::GLfloat) 0

  let localBeatCoeff sigIdx = do let sigs = DT.signalList signalBuf
                                 let sig = sigs !! ((length sigs) - sigIdx - 1)
                                 sigSamples <- getElems $ DT.signalArray sig
                                 let noisePerSample = 0.2
                                 let lbc = realToFrac $ (sum $ take maxBeatSamples sigSamples :: GLfloat) - (fromIntegral maxBeatSamples * noisePerSample) 
                                 return lbc 
  
  -- Render surface as single strips (for z : for x):
  color (Color4 0.7 0.2 0.7 surfOpacity :: Color4 GLfloat)
  let renderSignalSurfaceStrip vBuf nBuf count loudnessCoeff beatCoeff = (
        case vBuf of 
          (currVertices:nextVertices:xs) -> (
            do -- Local feature extraction (considering one signal at a time only): 
               lbc <- localBeatCoeff count
               
               let currNormals = nBuf !! (min count (length nBuf-1))
               let nextNormals = if length nBuf > (count+1) then nBuf !! (count+1) else currNormals
               
               -- Put vertices and normals in correct (interleaved) order for 
               -- rendendering: 
               let normals  = Conv.interleave currNormals nextNormals    -- [ n_0_0, n_1_0,  n_0_1, n_1_1, ... ]
               let vertices = Conv.interleave currVertices nextVertices  -- [ s_0_0, s_1_0,  s_0_1, s_1_1, ... ]
               -- Render a single surface strip (consisting of values from two signals): 
               renderSampleSurfaceStrip vertices normals count loudnessCoeff beatCoeff lbc
               
               -- recurse
               let recurse = if (count+1) < (length vBuf) then renderSignalSurfaceStrip (nextVertices:xs) nBuf (count+1) loudnessCoeff beatCoeff else return ()
               recurse
               return () )
          (sig:[]) -> return () 
          [] -> return () )
  
  -- Global feature extraction (considering all signals): 
  -- Analyze overall loudness: 
  let noisePerSample = 0.2
  let loudness sampleList = (sum sampleList) - (fromIntegral (length sampleList) * noisePerSample)
  let beatLoudness sampleList = (sum $ take maxBeatSamples sampleList) - (fromIntegral maxBeatSamples * noisePerSample)
  
  vertexBuf <- readIORef vertexBufIORef
  normalsBuf <- readIORef normalsBufIORef
  renderSignalSurfaceStrip vertexBuf normalsBuf 0 (loudness recentSignalSamples) (beatLoudness recentSignalSamples) 
--  GL.flush

reshape :: Gtk.Rectangle -> IO ()
reshape allocation = do
  let rectangleWidthHeight (Gtk.Rectangle _ _ w' h') = (w',h')
  let (w,h) = rectangleWidthHeight allocation
  viewport $= (Position 0 0, Size (fromIntegral w) (fromIntegral h))
  perspective (fromIntegral viewPerspective) (fromIntegral w / fromIntegral h) 0.1 100
  matrixMode $= Modelview 0
  return ()

-- Expects normals buffer, signal buffer and maximum number of signals in buffers. 
-- Returns updated normals buffer. 
updateNormalsBuffer :: [[ Normal3 GLfloat ]] -> [[ Vertex3 GLfloat ]] -> Int -> Int -> IO [[ Normal3 GLfloat ]] 
updateNormalsBuffer nBuf vBuf numSignals numSamples = do
  
  -- Drop first and last element of nBuf. 
  -- Last element has incomplete normals and will be replaced by element 
  -- with correct normals. This happens in any case. 
  -- Drop first element if max buffer size is exceeded. 
  let adjNormalsBuf = take (numSignals-2) (Conv.adjustBufferSize nBuf (numSignals-1))
  
  -- Take 3 most recent signals from vertex buffer. 
  -- Last element in signal buffer is most recent signal: 
  let sigCurrIdx  = 0
  let sigPrev1Idx = if length vBuf > 1 then 1 else sigCurrIdx
  let sigPrev2Idx = if length vBuf > 2 then 2 else sigPrev1Idx
  
  let sigCurr  = (vBuf !! sigCurrIdx) 
  let sigPrev1 = (vBuf !! sigPrev1Idx) 
  let sigPrev2 = (vBuf !! sigPrev2Idx) 
  
  -- Normal vector for current signal is incomplete: 
  let verticesAndNormalsCurr = RH.normalsFromVertices [sigPrev1,sigCurr,[]] numSamples 
  -- Normal vector for prev signal is complete: 
  let verticesAndNormalsPrev = RH.normalsFromVertices [sigPrev2,sigPrev1,sigCurr] numSamples 

  let updatedNormalsBuf = verticesAndNormalsPrev : verticesAndNormalsCurr : adjNormalsBuf 
  -- One element dropped at the end, two elements added at the end: 
  return updatedNormalsBuf

-- Component init interface for main UI. 
initComponent _ contextSettings = do

  window <- Gtk.windowNew

  Gtk.set window [ Gtk.containerBorderWidth := 8,
                   Gtk.windowTitle := "drool visualizer" ]

  putStrLn "Initializing OpenGL viewport"

  glConfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA, GtkGL.GLModeMultiSample,
                                 GtkGL.GLModeDouble, GtkGL.GLModeDepth, GtkGL.GLModeAlpha]
  _ <- GtkGL.initGL
  
  canvas <- GtkGL.glDrawingAreaNew glConfig
  
  settings <- readIORef contextSettings
  
  vertexBufIORef  <- newIORef []
  normalsBufIORef <- newIORef []

  renderSettings <- newIORef RH.RenderSettings { RH.xPosFun = (\x -> fromIntegral x / 60.0), 
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

    cullFace $= Just Front
    colorMaterial $= Just (Front, AmbientAndDiffuse)

    let blendModeSource = AC.blendModeSource cSettings
    let blendModeFrameBuffer = AC.blendModeFrameBuffer cSettings
    blendFunc $= (blendModeSource, blendModeFrameBuffer)
    
    -- blendFunc $= (SrcAlpha, OneMinusSrcAlpha) -- comic mode, fake cell shade
    -- blendFunc $= (SrcAlphaSaturate, One)
    -- blendFunc $= (OneMinusSrcColor, One)

    hint PerspectiveCorrection $= Nicest
    hint PolygonSmooth $= Nicest
    hint LineSmooth $= Nicest

{-  -- Black grid, cool beat illuminance
    cullFace $= Just Front
    colorMaterial $= Just (Front, AmbientAndDiffuse)
    blendFunc $= (SrcAlpha, DstAlpha)
-}

{-  -- Comic mode
    cullFace $= Just Front
    colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)
    blendFunc $= (SrcAlpha, OneMinusSrcAlpha)
-}

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


color3AddAlpha :: Color3 GLfloat -> GLfloat -> Color4 GLfloat
color3AddAlpha (Color3 r g b) a = Color4 r g b a

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
