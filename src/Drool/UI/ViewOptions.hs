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

import qualified Drool.Types as DT
import qualified Drool.ApplicationContext as AC
import qualified Drool.ContextObjects as AC

import qualified Drool.UI.GtkHelpers as GH
import qualified Drool.UI.Dialogs.FFTSurfaceDialog as FFTSurfaceDialog

import qualified Drool.UI.Visuals as Visuals

-- Initializes GUI component for view options.
-- Expects a GtkBuilder instance and default context settings. 
initComponent :: GtkBuilder.Builder -> IORef AC.ContextSettings -> IORef AC.ContextObjects -> IO Bool
initComponent gtkBuilder contextSettings contextObjects = do
  putStrLn "Initializing ViewOptions component"

  defaultSettings <- readIORef contextSettings

  button_view_perspectiveTop <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveTop"
  _ <- Gtk.onClicked button_view_perspectiveTop $ do
    
    visualInitState      <- Visuals.newFFTSurface contextSettings 
    visualInitStateIORef <- newIORef visualInitState
    cObjects <- readIORef contextObjects
    let visualIORef = AC.visual cObjects
    _ <- atomicModifyIORef visualIORef ( \_ -> (Visuals.newFFTSurfaceVisual contextSettings visualInitStateIORef,True) )

    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.renderPerspective = DT.Top }

  button_view_perspectiveFront <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonPerspectiveFront"
  _ <- Gtk.onClicked button_view_perspectiveFront $ do

    visualInitState      <- Visuals.newSpheres contextSettings 
    visualInitStateIORef <- newIORef visualInitState
    cObjects <- readIORef contextObjects
    let visualIORef = AC.visual cObjects
    _ <- atomicModifyIORef visualIORef ( \_ -> (Visuals.newSpheresVisual contextSettings visualInitStateIORef,True) )

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
  
  GH.bindAdjustment "adjAutoPerspectiveSwitchInterval" gtkBuilder contextSettings (\v settings -> settings { AC.autoPerspectiveSwitchInterval = round v }) 

  GH.bindAdjustment "adjViewAngle" gtkBuilder contextSettings (\v settings -> settings { AC.viewAngle = realToFrac v }) 
  GH.bindAdjustment "adjViewDistance" gtkBuilder contextSettings (\v settings -> settings { AC.viewDistance = realToFrac v }) 

  GH.bindAdjustment "adjXLinScale" gtkBuilder contextSettings (\v settings -> settings { AC.xLinScale = realToFrac v }) 
  GH.bindAdjustment "adjXLogScale" gtkBuilder contextSettings (\v settings -> settings { AC.xLogScale = realToFrac v }) 
  GH.bindAdjustment "adjZLinScale" gtkBuilder contextSettings (\v settings -> settings { AC.zLinScale = realToFrac v }) 

  GH.bindCheckButton "checkbuttonPlayback"      gtkBuilder contextSettings (\v settings -> settings { AC.playbackEnabled = v })
  GH.bindCheckButton "checkbuttonReverseBuffer" gtkBuilder contextSettings (\v settings -> settings { AC.reverseBuffer = v })

  -- Lights
  GH.bindCheckButton "checkbuttonLight1Enabled"  gtkBuilder contextSettings (\v settings -> settings { AC.light0 = (AC.light0 settings) { AC.lightState = if v then Enabled else Disabled } })
  GH.bindColorButton "colorbuttonLight1Ambient"  gtkBuilder contextSettings (\c s -> s { AC.light0 = (AC.light0 s) { AC.lightAmbient = c } } ) 
  GH.bindColorButton "colorbuttonLight1Diffuse"  gtkBuilder contextSettings (\c s -> s { AC.light0 = (AC.light0 s) { AC.lightDiffuse = c } } ) 
  GH.bindColorButton "colorbuttonLight1Specular" gtkBuilder contextSettings (\c s -> s { AC.light0 = (AC.light0 s) { AC.lightSpecular = c } } ) 
  GH.bindCheckButton "checkbuttonLight2Enabled"  gtkBuilder contextSettings (\v settings -> settings { AC.light1 = (AC.light1 settings) { AC.lightState = if v then Enabled else Disabled } })
  GH.bindColorButton "colorbuttonLight2Ambient"  gtkBuilder contextSettings (\c s -> s { AC.light1 = (AC.light1 s) { AC.lightAmbient = c } } ) 
  GH.bindColorButton "colorbuttonLight2Diffuse"  gtkBuilder contextSettings (\c s -> s { AC.light1 = (AC.light1 s) { AC.lightDiffuse = c } } ) 
  GH.bindColorButton "colorbuttonLight2Specular" gtkBuilder contextSettings (\c s -> s { AC.light1 = (AC.light1 s) { AC.lightSpecular = c } } ) 

  GH.bindButton "buttonVisualModelEdit" gtkBuilder contextSettings ( \_ -> do _ <- FFTSurfaceDialog.initComponent gtkBuilder 
                                                                                                                  contextSettings 
                                                                                                                  contextObjects 
                                                                              return () )
  
  return True

updateSettings :: GtkBuilder.Builder -> AC.ContextSettings -> IO Bool
updateSettings gtkBuilder settings = do 

  GH.initAdjustment "adjLinearScaling"  gtkBuilder (realToFrac $ AC.scaling settings)
  GH.initAdjustment "adjFixedRotationX" gtkBuilder (realToFrac $ DT.rotX $ AC.fixedRotation settings)
  GH.initAdjustment "adjFixedRotationY" gtkBuilder (realToFrac $ DT.rotY $ AC.fixedRotation settings)
  GH.initAdjustment "adjFixedRotationZ" gtkBuilder (realToFrac $ DT.rotZ $ AC.fixedRotation settings)
  GH.initAdjustment "adjIncRotationX"   gtkBuilder (realToFrac $ DT.rotX $ AC.incRotation settings)
  GH.initAdjustment "adjIncRotationY"   gtkBuilder (realToFrac $ DT.rotY $ AC.incRotation settings)
  GH.initAdjustment "adjIncRotationZ"   gtkBuilder (realToFrac $ DT.rotZ $ AC.incRotation settings)
  GH.initAdjustment "adjBandRange1Amp"  gtkBuilder (realToFrac $ (AC.rangeAmps settings) !! 0)
  GH.initAdjustment "adjBandRange2Amp"  gtkBuilder (realToFrac $ (AC.rangeAmps settings) !! 1)
  GH.initAdjustment "adjBandRange3Amp"  gtkBuilder (realToFrac $ (AC.rangeAmps settings) !! 2)
  GH.initAdjustment "adjBandRange4Amp"  gtkBuilder (realToFrac $ (AC.rangeAmps settings) !! 3)
  GH.initAdjustment "adjBandRange5Amp"  gtkBuilder (realToFrac $ (AC.rangeAmps settings) !! 4)

  GH.initCheckButton "checkbuttonPlayback" gtkBuilder (AC.playbackEnabled settings)
  
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
  
  GH.initAdjustment "adjFeatureSignalEnergySurfaceCoeff" gtkBuilder (realToFrac $ AC.featureSignalEnergySurfaceCoeff settings) 
  GH.initAdjustment "adjAutoPerspectiveSwitchInterval"   gtkBuilder (realToFrac $ AC.autoPerspectiveSwitchInterval settings) 
  
  GH.initCheckButton "checkbuttonLight1Enabled" gtkBuilder (if AC.lightState (AC.light0 settings) == Enabled then True else False)
  GH.initColorButton "colorbuttonLight1Ambient"  gtkBuilder (AC.lightAmbient $ AC.light0 settings)
  GH.initColorButton "colorbuttonLight1Diffuse"  gtkBuilder (AC.lightDiffuse $ AC.light0 settings)
  GH.initColorButton "colorbuttonLight1Specular" gtkBuilder (AC.lightSpecular $ AC.light0 settings)
  
  GH.initCheckButton "checkbuttonLight1Enabled" gtkBuilder (if AC.lightState (AC.light1 settings) == Enabled then True else False)
  GH.initColorButton "colorbuttonLight2Ambient"  gtkBuilder (AC.lightAmbient $ AC.light1 settings)
  GH.initColorButton "colorbuttonLight2Diffuse"  gtkBuilder (AC.lightDiffuse $ AC.light1 settings)
  GH.initColorButton "colorbuttonLight2Specular" gtkBuilder (AC.lightSpecular $ AC.light1 settings)

  return True

