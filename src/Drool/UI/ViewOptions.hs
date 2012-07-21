-----------------------------------------------------------------------------
--
-- Module      :  ViewOptions
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  Tobias Fuchs
-- Stability   :  experimental
-- Portability :  Win32, POSIX
--
-- |
--
-----------------------------------------------------------------------------

{-# OPTIONS -O2 -Wall #-}

module Drool.UI.ViewOptions (
  initComponent, updateSettings
) where

import Data.IORef

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder

import Graphics.Rendering.OpenGL

import qualified Drool.Utils.Conversions as Conv
import qualified Drool.Types as DT
import qualified Drool.ApplicationContext as AC

-- Initializes GUI component for view options.
-- Expects a GtkBuilder instance and default context settings. 
initComponent :: GtkBuilder.Builder -> IORef AC.ContextSettings -> IORef AC.ContextObjects -> IO Bool
initComponent gtkBuilder contextSettings _ = do
  putStrLn "Initializing ViewOptions component"

  defaultSettings <- readIORef contextSettings

  button_view_perspectiveTop <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveTop"
  _ <- Gtk.onClicked button_view_perspectiveTop $ do
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.renderPerspective = DT.Top }

  button_view_perspectiveFront <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveFront"
  _ <- Gtk.onClicked button_view_perspectiveFront $ do
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.renderPerspective = DT.Front }

  button_view_perspectiveSide <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveSide"
  _ <- Gtk.onClicked button_view_perspectiveSide $ do
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.renderPerspective = DT.Side }

  button_view_perspectiveIso <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveIsometric"
  _ <- Gtk.onClicked button_view_perspectiveIso $ do
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.renderPerspective = DT.Isometric }

  scale_view_linScalingAdj <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjLinearScaling"
  _ <- Gtk.onValueChanged scale_view_linScalingAdj $ do
    val <- Gtk.adjustmentGetValue scale_view_linScalingAdj
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.scaling = (realToFrac(val)::Float) }

  scale_view_gridOpacityAdj <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjGridOpacity"
  _ <- Gtk.onValueChanged scale_view_gridOpacityAdj $ do
    val <- Gtk.adjustmentGetValue scale_view_gridOpacityAdj
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.gridOpacity = (realToFrac(val)::GLfloat) }

  scale_view_surfaceOpacityAdj <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjSurfaceOpacity"
  _ <- Gtk.onValueChanged scale_view_surfaceOpacityAdj $ do
    val <- Gtk.adjustmentGetValue scale_view_surfaceOpacityAdj
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.surfaceOpacity = (realToFrac(val)::GLfloat) }

  colorbuttonGrid <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToColorButton "colorbuttonGrid"
  Gtk.colorButtonSetColor colorbuttonGrid (Conv.glColorToGtkColor $ AC.gridColor defaultSettings)
  _ <- Gtk.onColorSet colorbuttonGrid $ do
    gtkColor <- Gtk.colorButtonGetColor colorbuttonGrid
    let val = Conv.gtkColorToGLColor(gtkColor)
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.gridColor = val }

  colorbuttonSurface <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToColorButton "colorbuttonSurface"
  Gtk.colorButtonSetColor colorbuttonSurface (Conv.glColorToGtkColor $ AC.surfaceColor defaultSettings)
  _ <- Gtk.onColorSet colorbuttonSurface $ do
    gtkColor <- Gtk.colorButtonGetColor colorbuttonSurface
    let val = Conv.gtkColorToGLColor(gtkColor)
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.surfaceColor = val }

  colorbuttonLight <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToColorButton "colorbuttonLight"
  Gtk.colorButtonSetColor colorbuttonLight (Conv.glColorToGtkColor $ AC.lightColor defaultSettings)
  _ <- Gtk.onColorSet colorbuttonLight $ do
    gtkColor <- Gtk.colorButtonGetColor colorbuttonLight
    let val = Conv.gtkColorToGLColor(gtkColor)
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.lightColor = val }

  adjFixedRotationX <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjFixedRotationX"
  _ <- Gtk.onValueChanged adjFixedRotationX $ do
    val <- Gtk.adjustmentGetValue adjFixedRotationX
    settings <- readIORef contextSettings
    let cRotation = AC.fixedRotation settings
    contextSettings $=! settings { AC.fixedRotation = cRotation { DT.rotX = (realToFrac(val)::GLfloat) } }

  adjFixedRotationY <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjFixedRotationY"
  _ <- Gtk.onValueChanged adjFixedRotationY $ do 
    val <- Gtk.adjustmentGetValue adjFixedRotationY
    settings <- readIORef contextSettings
    let cRotation = AC.fixedRotation settings
    contextSettings $=! settings { AC.fixedRotation = cRotation { DT.rotY = (realToFrac(val)::GLfloat) } }

  adjFixedRotationZ <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjFixedRotationZ"
  _ <- Gtk.onValueChanged adjFixedRotationZ $ do 
    val <- Gtk.adjustmentGetValue adjFixedRotationZ
    settings <- readIORef contextSettings
    let cRotation = AC.fixedRotation settings
    contextSettings $=! settings { AC.fixedRotation = cRotation { DT.rotZ = (realToFrac(val)::GLfloat) } }

  adjIncRotationX <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjIncRotationX"
  _ <- Gtk.onValueChanged adjIncRotationX $ do
    val <- Gtk.adjustmentGetValue adjIncRotationX
    settings <- readIORef contextSettings
    let cRotation = AC.incRotation settings
    contextSettings $=! settings { AC.incRotation = cRotation { DT.rotX = (realToFrac(val)::GLfloat) } }

  adjIncRotationY <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjIncRotationY"
  _ <- Gtk.onValueChanged adjIncRotationY $ do 
    val <- Gtk.adjustmentGetValue adjIncRotationY
    settings <- readIORef contextSettings
    let cRotation = AC.incRotation settings
    contextSettings $=! settings { AC.incRotation = cRotation { DT.rotY = (realToFrac(val)::GLfloat) } }

  adjIncRotationZ <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjIncRotationZ"
  _ <- Gtk.onValueChanged adjIncRotationZ $ do 
    val <- Gtk.adjustmentGetValue adjIncRotationZ
    settings <- readIORef contextSettings
    let cRotation = AC.incRotation settings
    contextSettings $=! settings { AC.incRotation = cRotation { DT.rotZ = (realToFrac(val)::GLfloat) } }

  adjBandRange1Amp <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBandRange1Amp"
  _ <- Gtk.onValueChanged adjBandRange1Amp $ do 
    settings <- readIORef contextSettings
    dVal <- Gtk.adjustmentGetValue adjBandRange1Amp
    let fVal  = realToFrac dVal
    let cAmps = AC.rangeAmps settings
    let mAmps = fVal : (drop 1 cAmps)
    contextSettings $=! settings { AC.rangeAmps = mAmps }

  adjBandRange2Amp <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBandRange2Amp"
  _ <- Gtk.onValueChanged adjBandRange2Amp $ do 
    settings <- readIORef contextSettings
    dVal <- Gtk.adjustmentGetValue adjBandRange2Amp
    let fVal  = realToFrac dVal
    let cAmps = AC.rangeAmps settings
    let mAmps = take 1 cAmps ++ (fVal : (drop 2 cAmps))
    contextSettings $=! settings { AC.rangeAmps = mAmps }

  adjBandRange3Amp <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBandRange3Amp"
  _ <- Gtk.onValueChanged adjBandRange3Amp $ do 
    settings <- readIORef contextSettings
    dVal <- Gtk.adjustmentGetValue adjBandRange3Amp
    let fVal  = realToFrac dVal
    let cAmps = AC.rangeAmps settings
    let mAmps = take 2 cAmps ++ (fVal : (drop 3 cAmps))
    contextSettings $=! settings { AC.rangeAmps = mAmps }
  
  adjBandRange4Amp <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBandRange4Amp"
  _ <- Gtk.onValueChanged adjBandRange4Amp $ do 
    settings <- readIORef contextSettings
    dVal <- Gtk.adjustmentGetValue adjBandRange4Amp
    let fVal  = realToFrac dVal
    let cAmps = AC.rangeAmps settings
    let mAmps = take 3 cAmps ++ (fVal : (drop 4 cAmps))
    contextSettings $=! settings { AC.rangeAmps = mAmps }

  adjBandRange5Amp <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBandRange5Amp"
  _ <- Gtk.onValueChanged adjBandRange5Amp $ do 
    settings <- readIORef contextSettings
    dVal <- Gtk.adjustmentGetValue adjBandRange5Amp
    let fVal  = realToFrac dVal
    let cAmps = AC.rangeAmps settings
    let mAmps = take 4 cAmps ++ [fVal]
    contextSettings $=! settings { AC.rangeAmps = mAmps }

  comboboxBlendingSource <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToComboBox "comboboxBlendingSource"
  Gtk.comboBoxSetActive comboboxBlendingSource 4
  _ <- Gtk.on comboboxBlendingSource Gtk.changed $ do 
    settings <- readIORef contextSettings
    modeIdx <- Gtk.comboBoxGetActive comboboxBlendingSource
    contextSettings $=! settings { AC.blendModeSourceIdx = modeIdx } 

  comboboxBlendingFrameBuffer <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToComboBox "comboboxBlendingFrameBuffer"
  Gtk.comboBoxSetActive comboboxBlendingFrameBuffer 6
  _ <- Gtk.on comboboxBlendingFrameBuffer Gtk.changed $ do 
    settings <- readIORef contextSettings
    modeIdx <- Gtk.comboBoxGetActive comboboxBlendingFrameBuffer
    contextSettings $=! settings { AC.blendModeFrameBufferIdx = modeIdx } 

  _ <- updateSettings gtkBuilder defaultSettings

  comboboxFeatureBassEnergyTarget <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToComboBox "comboboxFeatureBassEnergyTarget"
  _ <- Gtk.on comboboxFeatureBassEnergyTarget Gtk.changed $ do 
    settings <- readIORef contextSettings
    targetIdx <- Gtk.comboBoxGetActive comboboxFeatureBassEnergyTarget
    contextSettings $=! settings { AC.featureBassEnergyTargetIdx = targetIdx } 

  comboboxFeatureSignalEnergyTarget <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToComboBox "comboboxFeatureSignalEnergyTarget"
  _ <- Gtk.on comboboxFeatureSignalEnergyTarget Gtk.changed $ do 
    settings <- readIORef contextSettings
    targetIdx <- Gtk.comboBoxGetActive comboboxFeatureSignalEnergyTarget
    contextSettings $=! settings { AC.featureSignalEnergyTargetIdx = targetIdx } 

  adjFeatureSignalEnergySurfaceCoeff <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjFeatureSignalEnergySurfaceCoeff"
  _ <- Gtk.onValueChanged adjFeatureSignalEnergySurfaceCoeff $ do
    settings <- readIORef contextSettings
    val <- Gtk.adjustmentGetValue adjFeatureSignalEnergySurfaceCoeff
    contextSettings $=! settings { AC.featureSignalEnergySurfaceCoeff = realToFrac val } 
  
  adjFeatureSignalEnergyGridCoeff <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjFeatureSignalEnergyGridCoeff"
  _ <- Gtk.onValueChanged adjFeatureSignalEnergyGridCoeff $ do
    settings <- readIORef contextSettings
    val <- Gtk.adjustmentGetValue adjFeatureSignalEnergyGridCoeff
    contextSettings $=! settings { AC.featureSignalEnergyGridCoeff = realToFrac val } 

  adjFeatureBassEnergySurfaceCoeff <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjFeatureBassEnergySurfaceCoeff"
  _ <- Gtk.onValueChanged adjFeatureBassEnergySurfaceCoeff $ do
    settings <- readIORef contextSettings
    val <- Gtk.adjustmentGetValue adjFeatureBassEnergySurfaceCoeff
    contextSettings $=! settings { AC.featureBassEnergySurfaceCoeff = realToFrac val } 
  
  adjFeatureBassEnergyGridCoeff <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjFeatureBassEnergyGridCoeff"
  _ <- Gtk.onValueChanged adjFeatureBassEnergyGridCoeff $ do
    settings <- readIORef contextSettings
    val <- Gtk.adjustmentGetValue adjFeatureBassEnergyGridCoeff
    contextSettings $=! settings { AC.featureBassEnergyGridCoeff = realToFrac val } 

  buttonSetMarquee <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonSetMarquee"
  entryMarquee <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToEntry "entryMarqueeText"
  _ <- Gtk.onClicked buttonSetMarquee $ do 
    marqueeText <- Gtk.entryGetText entryMarquee
 {- 
    textBuffer  <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToTextBuffer "textbufferMarquee"
    numChars    <- Gtk.textBufferGetCharCount textBuffer
    itBegin     <- Gtk.textBufferGetIterAtOffset textBuffer 0
    itEnd       <- Gtk.textBufferGetIterAtOffset textBuffer numChars
    marqueeText <- Gtk.textBufferGetText textBuffer itBegin itEnd False
 -}
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.marqueeText = marqueeText } 

  _ <- Gtk.afterEntryActivate entryMarquee $ do 
    marqueeText <- Gtk.entryGetText entryMarquee
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.marqueeText = marqueeText } 
    
  buttonToggleAutoPerspectiveSwitch <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToToggleButton "buttonToggleAutoPerspectiveSwitch"
  _ <- Gtk.onToggled buttonToggleAutoPerspectiveSwitch $ do
    state <- Gtk.toggleButtonGetActive buttonToggleAutoPerspectiveSwitch
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.autoPerspectiveSwitch = state } 
  
  adjAutoPerspectiveSwitchInterval <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjAutoPerspectiveSwitchInterval"
  _ <- Gtk.onValueChanged adjAutoPerspectiveSwitchInterval $ do
    val <- Gtk.adjustmentGetValue adjAutoPerspectiveSwitchInterval
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.autoPerspectiveSwitchInterval = round val } 

  adjViewAngle <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjViewAngle"
  _ <- Gtk.onValueChanged adjViewAngle $ do
    settings <- readIORef contextSettings
    val <- Gtk.adjustmentGetValue adjViewAngle
    contextSettings $=! settings { AC.viewAngle = realToFrac val } 

  adjViewDistance <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjViewDistance"
  _ <- Gtk.onValueChanged adjViewDistance $ do
    settings <- readIORef contextSettings
    val <- Gtk.adjustmentGetValue adjViewDistance
    contextSettings $=! settings { AC.viewDistance = realToFrac val } 

  adjXLinScale <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjXLinScale"
  _ <- Gtk.onValueChanged adjXLinScale $ do
    settings <- readIORef contextSettings
    val <- Gtk.adjustmentGetValue adjXLinScale
    contextSettings $=! settings { AC.xLinScale = realToFrac val } 

  adjXLogScale <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjXLogScale"
  _ <- Gtk.onValueChanged adjXLogScale $ do
    settings <- readIORef contextSettings
    val <- Gtk.adjustmentGetValue adjXLogScale
    contextSettings $=! settings { AC.xLogScale = realToFrac val } 

  adjZLinScale <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjZLinScale"
  _ <- Gtk.onValueChanged adjZLinScale $ do
    settings <- readIORef contextSettings
    val <- Gtk.adjustmentGetValue adjZLinScale
    contextSettings $=! settings { AC.zLinScale = realToFrac val } 
  
  checkbuttonPlayback <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToCheckButton "checkbuttonPlayback"
  _ <- Gtk.on checkbuttonPlayback Gtk.toggled $ do 
    val <- Gtk.toggleButtonGetActive checkbuttonPlayback
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.playbackEnabled = val }

  
  let updateCallback = ( do
      cSettings <- readIORef contextSettings
      let accIncRotation  = (AC.incRotationAccum cSettings) 
      let incRotationStep = (AC.incRotation cSettings) 
      let nextIncRotation = DT.CRotationVector { DT.rotY = (DT.rotY accIncRotation + DT.rotY incRotationStep), 
                                                 DT.rotX = (DT.rotX accIncRotation + DT.rotX incRotationStep), 
                                                 DT.rotZ = (DT.rotZ accIncRotation + DT.rotZ incRotationStep) } 
      modifyIORef contextSettings (\settings -> settings { AC.incRotationAccum = nextIncRotation } ) 
      return True )
--  updateTimer <- Gtk.timeoutAddFull updateCallback Gtk.priorityDefaultIdle (Conv.freqToMs 50)
  
  _ <- updateSettings gtkBuilder defaultSettings

  return True

updateSettings :: GtkBuilder.Builder -> AC.ContextSettings -> IO Bool
updateSettings gtkBuilder settings = do 
  scale_view_linScalingAdj <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjLinearScaling"
  Gtk.adjustmentSetValue scale_view_linScalingAdj (realToFrac $ AC.scaling settings)
  scale_view_gridOpacityAdj <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjGridOpacity"
  Gtk.adjustmentSetValue scale_view_gridOpacityAdj (realToFrac $ AC.gridOpacity settings)
  scale_view_surfaceOpacityAdj <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjSurfaceOpacity"
  Gtk.adjustmentSetValue scale_view_surfaceOpacityAdj (realToFrac $ AC.surfaceOpacity settings)
  adjFixedRotationX <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjFixedRotationX"
  Gtk.adjustmentSetValue adjFixedRotationX (realToFrac $ DT.rotX (AC.fixedRotation settings))
  adjFixedRotationY <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjFixedRotationY"
  Gtk.adjustmentSetValue adjFixedRotationY (realToFrac $ DT.rotY (AC.fixedRotation settings))
  adjFixedRotationZ <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjFixedRotationZ"
  Gtk.adjustmentSetValue adjFixedRotationZ (realToFrac $ DT.rotZ (AC.fixedRotation settings))
  adjIncRotationX <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjIncRotationX"
  Gtk.adjustmentSetValue adjIncRotationX (realToFrac $ DT.rotX (AC.incRotation settings))
  adjIncRotationY <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjIncRotationY"
  Gtk.adjustmentSetValue adjIncRotationY (realToFrac $ DT.rotY (AC.incRotation settings))
  adjIncRotationZ <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjIncRotationZ"
  Gtk.adjustmentSetValue adjIncRotationZ (realToFrac $ DT.rotZ (AC.incRotation settings))
  adjBandRange1Amp <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBandRange1Amp"
  Gtk.adjustmentSetValue adjBandRange1Amp (realToFrac $ (AC.rangeAmps settings) !! 0)
  adjBandRange2Amp <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBandRange2Amp"
  Gtk.adjustmentSetValue adjBandRange2Amp (realToFrac $ (AC.rangeAmps settings) !! 1)
  adjBandRange3Amp <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBandRange3Amp"
  Gtk.adjustmentSetValue adjBandRange3Amp (realToFrac $ (AC.rangeAmps settings) !! 2)
  adjBandRange4Amp <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBandRange4Amp"
  Gtk.adjustmentSetValue adjBandRange4Amp (realToFrac $ (AC.rangeAmps settings) !! 3)
  adjBandRange5Amp <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBandRange5Amp"
  Gtk.adjustmentSetValue adjBandRange5Amp (realToFrac $ (AC.rangeAmps settings) !! 4)

  checkbuttonPlayback <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToCheckButton "checkbuttonPlayback"
  Gtk.toggleButtonSetActive checkbuttonPlayback (AC.playbackEnabled settings)
  
  comboboxFeatureBassEnergyTarget <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToComboBox "comboboxFeatureBassEnergyTarget"
  Gtk.comboBoxSetActive comboboxFeatureBassEnergyTarget (AC.featureBassEnergyTargetIdx settings)
  comboboxFeatureSignalEnergyTarget <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToComboBox "comboboxFeatureSignalEnergyTarget"
  Gtk.comboBoxSetActive comboboxFeatureSignalEnergyTarget (AC.featureSignalEnergyTargetIdx settings)

  adjFeatureBassEnergyGridCoeff <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjFeatureBassEnergyGridCoeff"
  Gtk.adjustmentSetValue adjFeatureBassEnergyGridCoeff (realToFrac $ AC.featureBassEnergyGridCoeff settings)
  adjFeatureSignalEnergyGridCoeff <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjFeatureSignalEnergyGridCoeff"
  Gtk.adjustmentSetValue adjFeatureSignalEnergyGridCoeff (realToFrac $ AC.featureSignalEnergyGridCoeff settings)

  adjFeatureBassEnergySurfaceCoeff <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjFeatureBassEnergySurfaceCoeff"
  Gtk.adjustmentSetValue adjFeatureBassEnergySurfaceCoeff (realToFrac $ AC.featureBassEnergySurfaceCoeff settings)
  adjFeatureSignalEnergySurfaceCoeff <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjFeatureSignalEnergySurfaceCoeff"
  Gtk.adjustmentSetValue adjFeatureSignalEnergySurfaceCoeff (realToFrac $ AC.featureSignalEnergySurfaceCoeff settings)

  adjAutoPerspectiveSwitchInterval <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjAutoPerspectiveSwitchInterval"
  Gtk.adjustmentSetValue adjAutoPerspectiveSwitchInterval (fromIntegral $ AC.autoPerspectiveSwitchInterval settings)

  return True

