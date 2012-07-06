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
import qualified Drool.Utils.RenderHelpers as RH
import qualified Drool.Types as DT
import qualified Drool.ApplicationContext as AC


display :: IORef AC.ContextSettings -> IORef RH.RenderSettings -> IO ()
display contextSettingsIORef renderSettingsIORef = do
  clear [ColorBuffer]
  matrixMode $= Modelview 0
  loadIdentity

  renderSettings <- readIORef renderSettingsIORef
  settings <- readIORef contextSettingsIORef
  let hScale         = (AC.scaling settings) / (100.0::GLfloat)
      gridOpacity    = (AC.gridOpacity settings) / (100.0::GLfloat)
      surfOpacity    = (AC.surfaceOpacity settings) / (100.0::GLfloat)
      fixedRotation  = (AC.fixedRotation settings) 
      incRotation    = (AC.incRotation settings) 
      accIncRotation = (AC.incRotationAccum settings) 
      maxBeatSamples = (AC.maxBeatBandSamples settings) 
      vscale         = 2.0::GLfloat
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

  -- Load most recent signal from buffer (last signal in list): 
  let recentSignal = DT.getRecentSignal signalBuf 
  -- Get length of most recent signal (= number of samples per signal): 
  signalBounds <- getBounds $ DT.signalArray recentSignal
  let numSamples = rangeSize signalBounds
  -- Transform samples in recent signal: 
  recentSignalSamples <- getElems $ DT.signalArray recentSignal
  let rangeAmps = AC.rangeAmps settings
  let recentSignalSamplesTrans = RH.bandRangeAmpSamples recentSignalSamples rangeAmps
  -- Convert transformed samples of recent signal to vertices. 
  -- Use 0 for all z values: 
  let recentSignalVertices = RH.verticesFromSamples recentSignalSamplesTrans 0 renderSettings
  -- Push vertices of recent signal to vertex buffer. 
  -- Also updates z values of all vertices in vertex buffer: 
  let zPosFun = (\zIdx -> (fromIntegral zIdx) * signalLineDist)
  vBufCurr <- readIORef vertexBufIORef
  -- let vertexBufferPushedSignal = Conv.adjustBufferSize (vBufCurr ++ [recentSignalVertices]) numSignals
  let vertexBufferPushedSignal = recentSignalVertices : (Conv.adjustBufferSizeBack ( vBufCurr) (numSignals-1))
  let verticesWithAdjCoords = RH.updateVerticesZCoord vertexBufferPushedSignal zPosFun 
  modifyIORef vertexBufIORef ( \_ -> verticesWithAdjCoords )
  -- Now that all new vertices are ready, update the normals buffer: 
  nBufCurr <- readIORef normalsBufIORef
  updatedNormalsBuf <- updateNormalsBuffer nBufCurr verticesWithAdjCoords numSignals numSamples
  -- modifyIORef normalsBufIORef ( \buf -> if length updatedNormalsBuf > 0 then updatedNormalsBuf else buf )
  modifyIORef normalsBufIORef ( \_ -> updatedNormalsBuf )
  -- Update numSignals and numSamples: 
  modifyIORef renderSettingsIORef (\rs -> renderSettings { RH.numSignals = numSignals, RH.numSamples = numSamples } ) 
  
  ---------------------------------------------------------------------------------------------------
  -- End handling of new signal
  ---------------------------------------------------------------------------------------------------

  GL.translate $ Vector3 (-0.5 * vscale) 0 0

  GL.scale 1 hScale (1::GLfloat)

  GL.translate $ Vector3 0 0 (-(fromIntegral numSignals) * signalLineDist / 2.0)

{-
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
          [ Vertex3 (xVal) (v1*damp) (zVal::GLfloat),
            Vertex3 (xVal) (v2*damp) (zVal+signalLineDist::GLfloat) ] )
-}
  
  let renderSampleSurfaceStrip vertices normals idx lC bC lbC = do color surfaceColor
                                                                   renderPrimitive TriangleStrip (
                                                                     -- mapM_ GL.vertex (concat(tuplesToVertexList idx [0..numSamples] sTuples)) )
                                                                     -- mapM_ vertex vertices )
                                                                     mapM_ RH.vertexWithNormal ( zip vertices normals ) )
                                                                   -- color gridColor
                                                                   let beatDamping      = fromIntegral maxBeatSamples
                                                                   let loudnessDamping  = fromIntegral numSamples
                                                                   let localBeatDamping = fromIntegral maxBeatSamples
                                                                   let beatGridOpacity  = gridOpacity * ((bC / beatDamping) + (lC / loudnessDamping) + (lbC / localBeatDamping))
                                                                   let beatGridOpacity' = gridOpacity 
                                                                   color $ color3AddAlpha (AC.gridColor settings) beatGridOpacity' -- + bC / 100.0)
                                                                   -- translate $ Vector3 0 (0.05::GLfloat) 0
                                                                   renderPrimitive LineStrip ( 
                                                                     -- mapM_ GL.vertex (concat(tuplesToVertexList idx [0..numSamples] sTuples)) )
                                                                     mapM_ vertex vertices )
                                                                   -- translate $ Vector3 0 (-0.05::GLfloat) 0

  -- Render surface as single strips (for z : for x):
  color (Color4 0.7 0.2 0.7 surfOpacity :: Color4 GLfloat)
  let renderSignalSurfaceStrip vBuf nBuf count loudnessCoeff beatCoeff = (
        case vBuf of 
          (currVertices:nextVertices:xs) -> (
            -- Note that we only iterate over signal samples for feature extraction, 
            -- not for rendering. At this point, vertices are already computed and in 
            -- the vertex buffer. 
            -- Also, we are iterating from start to end of the signal list, but the most 
            -- recent signal is at the end of the list. This doesn't matter for rendering, 
            -- though. 
            do -- settings' <- readIORef contextSettingsIORef
               -- vBuf <- readIORef vBufIORef
               -- nBuf <- readIORef nBufIORef
               -- Local feature extraction (considering one signal at a time only): 
               -- sigSamples <- getElems $ DT.signalArray sig
               -- let noisePerSample = 0.2
               -- let localBeatCoeff = realToFrac $ (sum $ take maxBeatSamples sigSamples :: GLfloat) - (fromIntegral maxBeatSamples * noisePerSample)
               let localBeatCoeff = 0.8
               -- Load vertices and normals: 
               
               -- let currVertices = vBuf !! count
               -- let nextVertices = if length vBuf > (count+1) then vBuf !! (count+1) else currVertices
               let currNormals = nBuf !! (min count (length nBuf-1))
               let nextNormals = if length nBuf > (count+1) then nBuf !! (count+1) else currNormals
               
               -- Put vertices and normals in correct (interleaved) order for 
               -- rendendering: 
               let normals  = Conv.interleave currNormals nextNormals    -- [ n_0_0, n_1_0,  n_0_1, n_1_1, ... ]
               let vertices = Conv.interleave currVertices nextVertices  -- [ s_0_0, s_1_0,  s_0_1, s_1_1, ... ]
               -- Render a single surface strip (consisting of values from two signals): 
               renderSampleSurfaceStrip vertices normals count loudnessCoeff beatCoeff localBeatCoeff
               
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
  GL.flush

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
  -- let sigCurrIdx  = if length vBuf > 0 then (length vBuf)-1 else 0 
  -- let sigPrev1Idx = if length vBuf > 1 then (length vBuf)-2 else sigCurrIdx
  -- let sigPrev2Idx = if length vBuf > 2 then (length vBuf)-3 else sigPrev1Idx
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

  -- let updatedNormalsBuf = (adjNormalsBuf ++ [ verticesAndNormalsPrev, verticesAndNormalsCurr ] )
  let updatedNormalsBuf = verticesAndNormalsPrev : verticesAndNormalsCurr : adjNormalsBuf 
  -- One element dropped at the end, two elements added at the end: 
  -- putStrLn $ "updated normals: " ++ show updatedNormalsBuf
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

  renderSettings <- newIORef RH.RenderSettings { RH.xPosFun = (\x -> fromIntegral x / 50.0), 
                                                 RH.zPosFun = (\z -> fromIntegral z), 
                                                 RH.scaleFun = (\s _ _ -> s), 
                                                 RH.vertexBuf = vertexBufIORef, 
                                                 RH.normalsBuf = normalsBufIORef, 
                                                 RH.numSignals = 0, 
                                                 RH.numSamples = 0 } 

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
    cullFace $= Just Back
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

viewPerspective :: Int
viewPerspective = 90
