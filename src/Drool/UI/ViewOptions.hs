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
  initComponent
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
initComponent :: GtkBuilder.Builder -> IORef AC.ContextSettings -> IO Bool
initComponent gtkBuilder contextSettings = do
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
  Gtk.adjustmentSetValue scale_view_linScalingAdj (realToFrac $ AC.scaling defaultSettings)
  _ <- Gtk.onValueChanged scale_view_linScalingAdj $ do
    val <- Gtk.adjustmentGetValue scale_view_linScalingAdj
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.scaling = (realToFrac(val)::GLfloat) }

  scale_view_gridOpacityAdj <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjGridOpacity"
  Gtk.adjustmentSetValue scale_view_gridOpacityAdj (realToFrac $ AC.gridOpacity defaultSettings)
  _ <- Gtk.onValueChanged scale_view_gridOpacityAdj $ do
    val <- Gtk.adjustmentGetValue scale_view_gridOpacityAdj
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.gridOpacity = (realToFrac(val)::GLfloat) }

  scale_view_surfaceOpacityAdj <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjSurfaceOpacity"
  Gtk.adjustmentSetValue scale_view_surfaceOpacityAdj (realToFrac $ AC.surfaceOpacity defaultSettings)
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
  Gtk.adjustmentSetValue adjFixedRotationX (realToFrac $ DT.rotX (AC.fixedRotation defaultSettings))
  _ <- Gtk.onValueChanged adjFixedRotationX $ do
    val <- Gtk.adjustmentGetValue adjFixedRotationX
    settings <- readIORef contextSettings
    let cRotation = AC.fixedRotation settings
    contextSettings $=! settings { AC.fixedRotation = cRotation { DT.rotX = (realToFrac(val)::GLfloat) } }

  adjFixedRotationY <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjFixedRotationY"
  Gtk.adjustmentSetValue adjFixedRotationY (realToFrac $ DT.rotY (AC.fixedRotation defaultSettings))
  _ <- Gtk.onValueChanged adjFixedRotationY $ do 
    val <- Gtk.adjustmentGetValue adjFixedRotationY
    settings <- readIORef contextSettings
    let cRotation = AC.fixedRotation settings
    contextSettings $=! settings { AC.fixedRotation = cRotation { DT.rotY = (realToFrac(val)::GLfloat) } }

  adjFixedRotationZ <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjFixedRotationZ"
  Gtk.adjustmentSetValue adjFixedRotationZ (realToFrac $ DT.rotZ (AC.fixedRotation defaultSettings))
  _ <- Gtk.onValueChanged adjFixedRotationZ $ do 
    val <- Gtk.adjustmentGetValue adjFixedRotationZ
    settings <- readIORef contextSettings
    let cRotation = AC.fixedRotation settings
    contextSettings $=! settings { AC.fixedRotation = cRotation { DT.rotZ = (realToFrac(val)::GLfloat) } }

  adjIncRotationX <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjIncRotationX"
  Gtk.adjustmentSetValue adjIncRotationX (realToFrac $ DT.rotX (AC.incRotation defaultSettings))
  _ <- Gtk.onValueChanged adjIncRotationX $ do
    val <- Gtk.adjustmentGetValue adjIncRotationX
    settings <- readIORef contextSettings
    let cRotation = AC.incRotation settings
    contextSettings $=! settings { AC.incRotation = cRotation { DT.rotX = (realToFrac(val)::GLfloat) } }

  adjIncRotationY <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjIncRotationY"
  Gtk.adjustmentSetValue adjIncRotationY (realToFrac $ DT.rotY (AC.incRotation defaultSettings))
  _ <- Gtk.onValueChanged adjIncRotationY $ do 
    val <- Gtk.adjustmentGetValue adjIncRotationY
    settings <- readIORef contextSettings
    let cRotation = AC.incRotation settings
    contextSettings $=! settings { AC.incRotation = cRotation { DT.rotY = (realToFrac(val)::GLfloat) } }

  adjIncRotationZ <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjIncRotationZ"
  Gtk.adjustmentSetValue adjIncRotationZ (realToFrac $ DT.rotZ (AC.incRotation defaultSettings))
  _ <- Gtk.onValueChanged adjIncRotationZ $ do 
    val <- Gtk.adjustmentGetValue adjIncRotationZ
    settings <- readIORef contextSettings
    let cRotation = AC.incRotation settings
    contextSettings $=! settings { AC.incRotation = cRotation { DT.rotZ = (realToFrac(val)::GLfloat) } }

  adjBandRange1Amp <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBandRange1Amp"
  Gtk.adjustmentSetValue adjBandRange1Amp (realToFrac $ (AC.rangeAmps defaultSettings) !! 0)
  _ <- Gtk.onValueChanged adjBandRange1Amp $ do 
    settings <- readIORef contextSettings
    dVal <- Gtk.adjustmentGetValue adjBandRange1Amp
    let fVal  = realToFrac dVal
    let cAmps = AC.rangeAmps settings
    let mAmps = fVal : (drop 1 cAmps)
    contextSettings $=! settings { AC.rangeAmps = mAmps }

  adjBandRange2Amp <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBandRange2Amp"
  Gtk.adjustmentSetValue adjBandRange2Amp (realToFrac $ (AC.rangeAmps defaultSettings) !! 1)
  _ <- Gtk.onValueChanged adjBandRange2Amp $ do 
    settings <- readIORef contextSettings
    dVal <- Gtk.adjustmentGetValue adjBandRange2Amp
    let fVal  = realToFrac dVal
    let cAmps = AC.rangeAmps settings
    let mAmps = take 1 cAmps ++ (fVal : (drop 2 cAmps))
    contextSettings $=! settings { AC.rangeAmps = mAmps }

  adjBandRange3Amp <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBandRange3Amp"
  Gtk.adjustmentSetValue adjBandRange3Amp (realToFrac $ (AC.rangeAmps defaultSettings) !! 2)
  _ <- Gtk.onValueChanged adjBandRange3Amp $ do 
    settings <- readIORef contextSettings
    dVal <- Gtk.adjustmentGetValue adjBandRange3Amp
    let fVal  = realToFrac dVal
    let cAmps = AC.rangeAmps settings
    let mAmps = take 2 cAmps ++ (fVal : (drop 3 cAmps))
    contextSettings $=! settings { AC.rangeAmps = mAmps }
  
  adjBandRange4Amp <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBandRange4Amp"
  Gtk.adjustmentSetValue adjBandRange4Amp (realToFrac $ (AC.rangeAmps defaultSettings) !! 3)
  _ <- Gtk.onValueChanged adjBandRange4Amp $ do 
    settings <- readIORef contextSettings
    dVal <- Gtk.adjustmentGetValue adjBandRange4Amp
    let fVal  = realToFrac dVal
    let cAmps = AC.rangeAmps settings
    let mAmps = take 3 cAmps ++ (fVal : (drop 4 cAmps))
    contextSettings $=! settings { AC.rangeAmps = mAmps }

  adjBandRange5Amp <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBandRange5Amp"
  Gtk.adjustmentSetValue adjBandRange5Amp (realToFrac $ (AC.rangeAmps defaultSettings) !! 4)
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
    let mode = blendModeSourceFromIndex modeIdx
    contextSettings $=! settings { AC.blendModeSource = mode } 

  comboboxBlendingFrameBuffer <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToComboBox "comboboxBlendingFrameBuffer"
  Gtk.comboBoxSetActive comboboxBlendingFrameBuffer 6
  _ <- Gtk.on comboboxBlendingFrameBuffer Gtk.changed $ do 
    settings <- readIORef contextSettings
    modeIdx <- Gtk.comboBoxGetActive comboboxBlendingFrameBuffer
    let mode = blendModeFrameBufferFromIndex modeIdx
    contextSettings $=! settings { AC.blendModeFrameBuffer = mode } 
    
  
  return True

blendModeSourceFromIndex :: Int -> BlendingFactor
blendModeSourceFromIndex i = case i of 
                               0 -> Zero
                               1 -> One
                               2 -> DstColor
                               3 -> OneMinusDstColor
                               4 -> SrcAlpha
                               5 -> OneMinusSrcAlpha
                               6 -> DstAlpha
                               7 -> OneMinusDstAlpha
                               8 -> SrcAlphaSaturate
                               _ -> error "Unknown source blending mode"

blendModeFrameBufferFromIndex :: Int -> BlendingFactor
blendModeFrameBufferFromIndex i = case i of 
                               0 -> Zero
                               1 -> One
                               2 -> SrcColor
                               3 -> OneMinusSrcColor
                               4 -> SrcAlpha
                               5 -> OneMinusSrcAlpha
                               6 -> DstAlpha
                               7 -> OneMinusDstAlpha
                               _ -> error "Unknown frame buffer blending mode"

