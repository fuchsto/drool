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
import qualified Graphics.UI.GLUT as GLUT ( initialize )
import qualified Graphics.UI.Gtk.OpenGL as GtkGL

import qualified Drool.Utils.SigGen as SigGen ( SignalGenerator(..) )
import qualified Drool.Utils.Conversions as Conv
import qualified Drool.Utils.RenderHelpers as RH
import qualified Drool.Utils.FeatureExtraction as FE ( SignalFeaturesList(..), FeatureTarget(..) )
import qualified Drool.Types as DT
import qualified Drool.ApplicationContext as AC
import qualified Drool.ContextObjects as AC

import qualified Control.Monad as M ( forever, forM ) 
import qualified Control.Concurrent as C
import qualified Control.Concurrent.MVar as MV ( MVar, swapMVar, takeMVar, putMVar )
import qualified Control.Concurrent.Chan as CC ( readChan )

import qualified Drool.UI.Visuals as Visuals

import Graphics.UI.GLUT ( Object(Sphere'), renderObject, Flavour(..) )


display :: IORef AC.ContextSettings -> IORef RH.RenderSettings -> [ IORef (Visuals.Visual) ] -> IO ()
display contextSettingsIORef renderSettingsIORef visualIORefs = do
  clear [ColorBuffer, DepthBuffer]
  _ <- M.forM visualIORefs ( \visualIORef -> preservingMatrix $ do
                                               displayModel contextSettingsIORef renderSettingsIORef visualIORef )

  return ()

displayModel :: IORef AC.ContextSettings -> IORef RH.RenderSettings -> IORef (Visuals.Visual) -> IO ()
displayModel contextSettingsIORef renderSettingsIORef visualIORef = do
-- {{{
--  renderSettingsPrev <- readIORef renderSettingsIORef
  contextSettings    <- readIORef contextSettingsIORef
  renderSettingsPrev <- readIORef renderSettingsIORef

  visual <- readIORef visualIORef

  let timeoutMs = (Conv.freqToMs $ AC.renderingFrequency contextSettings)
  let tick      = RH.tick renderSettingsPrev
  let tickMs    = tick * timeoutMs

  let renderSettingsCurr = renderSettingsPrev { RH.tick = (tick+1) `mod` 1000 }

  let samplingSem  = RH.samplingSem renderSettingsCurr

  signalBuf   <- readIORef (RH.signalBuf renderSettingsCurr)
  featuresBuf <- readIORef (RH.featuresBuf renderSettingsCurr)

  let featuresCurr = head (FE.signalFeaturesList featuresBuf)

  -- Wait until there is at least one signal ready for rendering. 
  let nNewSignals = RH.numNewSignals renderSettingsCurr
  
  -- Load most recent signal from buffer (last signal in list): 
  let recentSignal = DT.getRecentSignal signalBuf 
  let lastSignal   = DT.getLastSignal signalBuf 
  -- Get length of most recent signal (= number of samples per signal): 
  numSamplesCurr <- case recentSignal of 
                         Just s  -> do signalBounds <- getBounds $ DT.signalArray s
                                       return $ rangeSize signalBounds
                         Nothing -> return 0 
  numSamplesLast <- case lastSignal of 
                         Just s  -> do signalBounds <- getBounds $ DT.signalArray s
                                       return $ rangeSize signalBounds
                         Nothing -> return 0 

  let nSamples = min numSamplesCurr numSamplesLast
  let nSignals = length $ DT.signalList signalBuf

  modifyIORef renderSettingsIORef ( \_ -> renderSettingsCurr { RH.numSignals    = nSignals, 
                                                               RH.numNewSignals = nNewSignals, 
                                                               RH.numSamples    = nSamples } )
  renderSettings <- readIORef renderSettingsIORef
  
  let accIncRotation  = (AC.incRotationAccum contextSettings) 
  let incRotationStep = (AC.incRotation contextSettings) 
  let nextIncRotation = DT.CRotationVector { DT.rotY = (DT.rotY accIncRotation + DT.rotY incRotationStep), 
                                             DT.rotX = (DT.rotX accIncRotation + DT.rotX incRotationStep), 
                                             DT.rotZ = (DT.rotZ accIncRotation + DT.rotZ incRotationStep) } 
  modifyIORef contextSettingsIORef (\settings -> settings { AC.incRotationAccum = nextIncRotation } ) 
  
  -- Push new signal(s) to visual: 

  (Visuals.update visual) renderSettings tick
  visualUpdated <- readIORef visualIORef
  
  matrixMode $= Projection
  loadIdentity
  perspective (realToFrac (AC.viewAngle contextSettings)) (fromIntegral canvasInitWidth / fromIntegral canvasInitHeight) 0.1 100
  
  matrixMode $= Modelview 0
  loadIdentity

  let hScale         = (AC.scaling contextSettings) / (100.0::Float)
      surfOpacity    = (AC.surfaceOpacity contextSettings) / (100.0::GLfloat)
      fixedRotation  = AC.fixedRotation contextSettings
      accIncRotation = AC.incRotationAccum contextSettings
      viewDistance   = AC.viewDistance contextSettings
      maxNumSignals  = AC.signalBufferSize contextSettings
      lightPos0      = RH.lightPos0 renderSettingsCurr
      lightPos1      = RH.lightPos1 renderSettingsCurr
      vPerspective   = AC.renderPerspective contextSettings

  let updatePerspective p = if AC.autoPerspectiveSwitch contextSettings && tickMs >= AC.autoPerspectiveSwitchInterval contextSettings then ( do 
                                let nextPerspective = RH.nextPerspective p
                                modifyIORef contextSettingsIORef ( \_ -> contextSettings { AC.renderPerspective = nextPerspective } )
                                modifyIORef renderSettingsIORef ( \_ -> renderSettingsCurr { RH.tick = 0 } )
                                return nextPerspective )
                            else return p
  curPerspective <- updatePerspective vPerspective


  let blendModeSource      = Conv.blendModeSourceFromIndex $ AC.blendModeSourceIdx contextSettings
  let blendModeFrameBuffer = Conv.blendModeFrameBufferFromIndex $ AC.blendModeFrameBufferIdx contextSettings
  
  blendFunc $= (blendModeSource, blendModeFrameBuffer)

  ---------------------------------------------------------------------------------------------------
  -- Render background
  ---------------------------------------------------------------------------------------------------

{-  
  matrixMode $= Modelview 0
  preservingMatrix $ do 
    GL.translate $ Vector3 0 0 (-5.0 :: GLfloat)
    let gMaterial = AC.surfaceMaterial contextSettings
        gOpacity  = 0.9
    materialAmbient   FrontAndBack $= RH.color4MulAlpha (AC.materialAmbient gMaterial) gOpacity
    materialDiffuse   FrontAndBack $= RH.color4MulAlpha (AC.materialDiffuse gMaterial) gOpacity
    materialSpecular  FrontAndBack $= RH.color4MulAlpha (AC.materialSpecular gMaterial) gOpacity
    materialEmission  FrontAndBack $= RH.color4MulAlpha (AC.materialEmission gMaterial) gOpacity
    materialShininess FrontAndBack $= AC.materialShininess gMaterial
    renderObject Solid (Sphere' 2.0 50 50)
    renderObject Wireframe (Sphere' (2.0 * 1.01) 40 40)
-}
  clear [DepthBuffer]

  preservingMatrix $ do

    let lightIntensity = lCoeff + bCoeff 
          where (lCoeff,bCoeff) = RH.featuresToIntensity featuresCurr FE.GlobalTarget contextSettings

    RH.useLight $ (AC.light0 contextSettings) { AC.lightIntensity = lightIntensity }
    RH.useLight $ (AC.light1 contextSettings) { AC.lightIntensity = lightIntensity }
    GL.position (Light 0) $= lightPos0
    GL.position (Light 1) $= lightPos1
    
    ---------------------------------------------------------------------------------------------------
    -- Perspective transformations
    ---------------------------------------------------------------------------------------------------

    GL.translate $ Vector3 0 0 viewDistance

    RH.applyPerspective curPerspective
    RH.applyGlobalRotation fixedRotation accIncRotation

    ---------------------------------------------------------------------------------------------------
    -- End of perspective transformations
    ---------------------------------------------------------------------------------------------------

    (visWidth,visHeight,visDepth) <- (Visuals.dimensions visualUpdated) 
    
    -- fogMode  $= Linear 0.0 (visDepth * 20.0)
    -- fogColor $= (Color4 0.0 0.0 0.0 1.0)

    -- GL.translate $ Vector3 (-0.5 * visWidth) 0 0
    -- GL.translate $ Vector3 0 0 (-0.5 * visDepth)

    Visuals.render visualUpdated

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

-- Component init interface for main UI. 
initComponent :: GtkBuilder.Builder -> IORef AC.ContextSettings -> IORef AC.ContextObjects -> IO ()
initComponent _ contextSettingsIORef contextObjectsIORef = do
-- {{{
  window <- Gtk.windowNew

  _ <- GLUT.initialize "drool visualizer" []

  Gtk.set window [ Gtk.containerBorderWidth := 0,
                   Gtk.windowTitle := "drool visualizer" ]

  putStrLn "Initializing OpenGL viewport"

  glConfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA, GtkGL.GLModeMultiSample, GtkGL.GLModeStencil, 
                                 GtkGL.GLModeDouble, GtkGL.GLModeDepth, GtkGL.GLModeAlpha]
  _ <- GtkGL.initGL
  
  canvas <- GtkGL.glDrawingAreaNew glConfig

  cObjects   <- readIORef contextObjectsIORef
  cSettings  <- readIORef contextSettingsIORef  
  let sigGen  = AC.signalGenerator cObjects
  
  let renderSettings = RH.RenderSettings { RH.signalGenerator   = sigGen, 
                                           RH.samplingSem       = AC.samplingSem cObjects, 
                                           RH.numNewSignalsChan = AC.numNewSignalsChan cObjects, 
                                           RH.signalBuf         = AC.signalBuf cObjects, 
                                           RH.featuresBuf       = AC.featuresBuf cObjects, 
                                           RH.lightPos0         = (Vertex4 (-1.0) 3.0 (-2.0) 0.0), 
                                           RH.lightPos1         = (Vertex4 1.0 3.0 2.0 0.0), 
                                           RH.numSignals        = 0, 
                                           RH.numNewSignals     = 0, 
                                           RH.numSamples        = SigGen.numSamples sigGen, 
                                           RH.tick              = 0 }
  
  renderSettingsIORef <- newIORef renderSettings

  let visualForegroundIORef   = AC.visualForeground cObjects
      visualMiddlegroundIORef = AC.visualMiddleground cObjects
      visualBackgroundIORef   = AC.visualBackground cObjects
  
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
    -- polygonSmooth $= Enabled
    -- lineSmooth $= Enabled
    lighting $= Enabled
    light (Light 0) $= Enabled
    light (Light 1) $= Enabled
    frontFace $= CCW
    blend $= Enabled
    multisample $= Enabled
    sampleAlphaToCoverage $= Enabled
    -- fog $= Enabled

    lineWidthRange <- GL.get smoothLineWidthRange
    lineWidth $= fst lineWidthRange -- use thinnest possible lines

    colorMaterial $= Just (FrontAndBack, AmbientAndDiffuse)

    let blendModeSource = Conv.blendModeSourceFromIndex $ AC.blendModeSourceIdx cSettings
    let blendModeFrameBuffer = Conv.blendModeFrameBufferFromIndex $ AC.blendModeFrameBufferIdx cSettings
    blendFunc $= (blendModeSource, blendModeFrameBuffer)
    
    hint PerspectiveCorrection $= Nicest
    -- hint PolygonSmooth $= Nicest
    -- hint LineSmooth $= Nicest

    matrixMode $= Projection
    loadIdentity
    viewport $= (Position 0 0, Size (fromIntegral canvasInitWidth) (fromIntegral canvasInitHeight))
    perspective (realToFrac $ AC.viewAngle cSettings) (fromIntegral canvasInitWidth / fromIntegral canvasInitHeight) 0.1 10

    matrixMode $= Modelview 0
    loadIdentity
  
    return ()
    -- }}}

  -- OnShow handler for GL canvas:
  _ <- Gtk.onExpose canvas $ \_ -> do
    GtkGL.withGLDrawingArea canvas $ \glwindow -> do
      let visualModels = [ visualBackgroundIORef, visualMiddlegroundIORef, visualForegroundIORef ]
      display contextSettingsIORef renderSettingsIORef visualModels
      GtkGL.glDrawableSwapBuffers glwindow
    return True

  -- Resize handler:
  _ <- Gtk.onSizeAllocate canvas (reshape contextSettingsIORef)

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

  let timeoutMs = (Conv.freqToMs $ AC.renderingFrequency cSettings)

{-
  -- Redraw canvas according to rendering frequency:
  updateCanvasTimer <- Gtk.timeoutAddFull (do
      Gtk.widgetQueueDraw canvas
      return True)
    Gtk.priorityDefaultIdle timeoutMs
  -- Remove timer for redrawing canvas when closing window:
  _ <- Gtk.onDestroy window (Gtk.timeoutRemove updateCanvasTimer)
-}

  -- Try to redraw canvas on every new signal: 
  sampleThread <- C.forkOS . M.forever $ do nNewSignals <- MV.takeMVar $ AC.samplingSem cObjects
                                            modifyIORef renderSettingsIORef ( \rs -> rs { RH.numNewSignals = nNewSignals } )
                                            Gtk.postGUISync $ Gtk.widgetQueueDraw canvas
    


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

