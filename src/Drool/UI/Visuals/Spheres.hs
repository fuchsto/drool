-----------------------------------------------------------------------------
--
-- Module      :  Drool.UI.Visuals.Spheres
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

module Drool.UI.Visuals.Spheres (
    Spheres, -- hidden type constructor
    createSpheresVisual, 
    spheresNew, 
    spheresDimensions, 
    spheresRender, 
    spheresUpdate
) where

-- Imports
-- {{{
import Drool.UI.Visuals.Visual as Visual

import Data.IORef ( IORef, newIORef, readIORef, modifyIORef )
import qualified Drool.Utils.RenderHelpers as RH ( RenderSettings(..), color4MulAlpha )
import qualified Drool.ApplicationContext as AC ( ContextSettings(..), MaterialConfig(..) )
import qualified Drool.Utils.SigGen as SigGen ( SignalGenerator(..) )
import Drool.Utils.FeatureExtraction as FE ( 
    SignalFeatures(..), SignalFeaturesList(..), 
    FeatureTarget(..), featureTargetFromIndex )

import Graphics.UI.GLUT ( Object(Sphere'), renderObject, Flavour(..) )
import qualified Graphics.UI.GLUT as GLUT ( initialize )
import Graphics.Rendering.OpenGL as GL ( 
    ($=), 
    GLfloat, 
    Face(..), 
    materialEmission, 
    materialAmbient, 
    materialDiffuse, 
    materialSpecular,
    materialShininess,
    colorMaterial, 
    ColorMaterialParameter(..) )
-- }}}

data Spheres = Spheres { contextSettings :: AC.ContextSettings, 
                         renderSettings  :: RH.RenderSettings, 
                         gridMaterial    :: AC.MaterialConfig, 
                         surfaceMaterial :: AC.MaterialConfig, 
                         gridOpacity     :: GLfloat, 
                         surfaceOpacity  :: GLfloat, 
                         radius          :: GLfloat, 
                         numSamples      :: Int }

-- Hook Visual interface function definitions to concrete implementations: 
createSpheresVisual :: IORef AC.ContextSettings -> Visual Spheres
createSpheresVisual contextSettingsIORef = Visual { -- curried: RenderSettings -> IO (Spheres) 
                                                    newVisual  = spheresNew contextSettingsIORef, 
                                                    -- curried: Spheres
                                                    dimensions = spheresDimensions, 
                                                    -- curried: RenderSettings -> IORef Spheres -> Int -> IO (Spheres)
                                                    update     = spheresUpdate contextSettingsIORef, 
                                                    -- curried: Spheres 
                                                    render     = spheresRender }

spheresNew :: IORef AC.ContextSettings -> RH.RenderSettings -> IO (Spheres)
-- {{{
spheresNew cSettingsIORef rSettings = do
  cSettings <- readIORef cSettingsIORef
  
  _ <- GLUT.initialize "spheres" []

  let sigGen   = RH.signalGenerator rSettings
  let nSamples = SigGen.numSamples sigGen
  
  let settings = Spheres { contextSettings = cSettings, 
                           renderSettings  = rSettings, 
                           gridMaterial    = undefined, 
                           surfaceMaterial = undefined,
                           gridOpacity     = undefined, 
                           surfaceOpacity  = undefined, 
                           radius          = 1, 
                           numSamples      = nSamples }
  return settings
-- }}}

spheresDimensions :: Spheres -> (GLfloat,GLfloat,GLfloat)
-- {{{
spheresDimensions visual = (width,height,depth)
  where width  = r * 2.0
        height = r * 2.0
        depth  = r * 2.0
        r      = radius visual
-- }}}

spheresUpdate :: IORef AC.ContextSettings -> RH.RenderSettings -> IORef Spheres -> Int -> IO (Spheres)
-- {{{
spheresUpdate cSettingsIORef rSettings visualIORef t = do
  cSettings <- readIORef cSettingsIORef

  visualPrev <- readIORef visualIORef 

  let target    = FE.LocalTarget
  
  fBuf <- readIORef $ RH.featuresBuf rSettings
  let features = head (FE.signalFeaturesList fBuf)
  
  let loudness     = realToFrac $ FE.totalEnergy features
      basslevel    = realToFrac $ FE.bassEnergy features 
      lTarget      = FE.featureTargetFromIndex $ AC.featureSignalEnergyTargetIdx cSettings
      bTarget      = FE.featureTargetFromIndex $ AC.featureBassEnergyTargetIdx cSettings
      lCoeff       = if lTarget == target || target == FE.GlobalAndLocalTarget then (
                        realToFrac $ AC.featureSignalEnergyGridCoeff cSettings )
                     else 0.0
      bCoeff       = if bTarget == target || target == FE.GlobalAndLocalTarget then (
                        realToFrac $ AC.featureBassEnergyGridCoeff cSettings )
                     else 0.0 
  let gBaseOpacity = (AC.gridOpacity cSettings) / 100.0 :: GLfloat
      gOpacity     = gBaseOpacity + (lCoeff * loudness) + (bCoeff * basslevel)
      gMaterial    = AC.gridMaterial cSettings

  let sBaseOpacity = (AC.surfaceOpacity cSettings) / 100.0 :: GLfloat
      sOpacity     = sBaseOpacity + (lCoeff * loudness) + (bCoeff * basslevel)
      sMaterial    = AC.surfaceMaterial cSettings

  let newRadius    = realToFrac $ 0.3 + (lCoeff * loudness) + (bCoeff * basslevel) 

  let visual = visualPrev { renderSettings  = rSettings, 
                            contextSettings = cSettings, 
                            gridMaterial    = gMaterial, 
                            surfaceMaterial = sMaterial,
                            gridOpacity     = gOpacity, 
                            surfaceOpacity  = sOpacity, 
                            radius          = newRadius } 

  modifyIORef visualIORef ( \_ -> visual )
  return visual
-- }}}

spheresRender :: Spheres -> IO ()
-- {{{
spheresRender visual = do 
  let r         = realToFrac $ radius visual
      gMaterial = gridMaterial visual
      gOpacity  = gridOpacity visual
      sMaterial = surfaceMaterial visual
      sOpacity  = surfaceOpacity visual

  materialAmbient   FrontAndBack $= RH.color4MulAlpha (AC.materialAmbient gMaterial) gOpacity
  materialDiffuse   FrontAndBack $= RH.color4MulAlpha (AC.materialDiffuse gMaterial) gOpacity
  materialSpecular  FrontAndBack $= RH.color4MulAlpha (AC.materialSpecular gMaterial) gOpacity
  materialEmission  FrontAndBack $= RH.color4MulAlpha (AC.materialEmission gMaterial) gOpacity
  materialShininess FrontAndBack $= AC.materialShininess gMaterial
  renderObject Wireframe (Sphere' (r * 1.01) 40 40)

  materialAmbient   FrontAndBack $= RH.color4MulAlpha (AC.materialAmbient sMaterial) sOpacity
  materialDiffuse   FrontAndBack $= RH.color4MulAlpha (AC.materialDiffuse sMaterial) sOpacity
  materialSpecular  FrontAndBack $= RH.color4MulAlpha (AC.materialSpecular sMaterial) sOpacity
  materialEmission  FrontAndBack $= RH.color4MulAlpha (AC.materialEmission sMaterial) sOpacity
  materialShininess FrontAndBack $= AC.materialShininess sMaterial
  renderObject Solid (Sphere' r 50 50)
-- }}}
