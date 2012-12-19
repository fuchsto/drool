-----------------------------------------------------------------------------
--
-- Module      :  Drool.UI.Visuals.Tunnel
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

module Drool.UI.Visuals.Tunnel (
    TunnelState, -- hidden type constructor
    newTunnelVisual, 
    newTunnelState
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

data TunnelState = TunnelState { contextSettings :: AC.ContextSettings, 
                                 renderSettings  :: RH.RenderSettings, 
                                 gridMaterial    :: AC.MaterialConfig, 
                                 surfaceMaterial :: AC.MaterialConfig, 
                                 gridOpacity     :: GLfloat, 
                                 surfaceOpacity  :: GLfloat, 
                                 radius          :: GLfloat, 
                                 numSamples      :: Int }

-- Hook Visual state IORef to concrete implementations: 
newTunnelVisual :: IORef AC.ContextSettings -> IORef TunnelState -> Visual
newTunnelVisual contextSettingsIORef stateIORef = Visual { dimensions = tunnelDimensions stateIORef, 
                                                           update     = tunnelUpdate contextSettingsIORef stateIORef, 
                                                           render     = tunnelRender stateIORef }


newTunnelState :: IORef AC.ContextSettings -> IO TunnelState
-- {{{
newTunnelState cSettingsIORef = do
  cSettings <- readIORef cSettingsIORef
  let settings = TunnelState { contextSettings = cSettings, 
                               renderSettings  = undefined, 
                               gridMaterial    = undefined, 
                               surfaceMaterial = undefined,
                               gridOpacity     = undefined, 
                               surfaceOpacity  = undefined, 
                               radius          = 1, 
                               numSamples      = 0 }
  return settings
-- }}}

tunnelDimensions :: IORef TunnelState -> IO (GLfloat,GLfloat,GLfloat)
-- {{{
tunnelDimensions visualIORef = do
  visual <- readIORef $ visualIORef
  let width  = r * 2.0
      height = r * 2.0
      depth  = r * 2.0
      r      = radius visual
  return (width,height,depth)
-- }}}

tunnelUpdate :: IORef AC.ContextSettings -> IORef TunnelState -> RH.RenderSettings -> Int -> IO ()
-- {{{
tunnelUpdate cSettingsIORef visualIORef rSettings t = do
  cSettings <- readIORef cSettingsIORef

  visualPrev <- readIORef visualIORef 

  let target    = FE.LocalTarget
  
  fBuf <- readIORef $ RH.featuresBuf rSettings
  let features = head (FE.signalFeaturesList fBuf)
  
  let loudness     = realToFrac $ FE.totalEnergy features
      basslevel    = realToFrac $ FE.bassEnergy features 
      lTarget      = FE.featureTargetFromIndex $ AC.featureSignalEnergyTargetIdx cSettings
      bTarget      = FE.featureTargetFromIndex $ AC.featureBassEnergyTargetIdx cSettings
      lCoeff       = if lTarget == target || lTarget == FE.GlobalAndLocalTarget then (
                        realToFrac $ AC.featureSignalEnergyGridCoeff cSettings )
                     else 0.0
      bCoeff       = if bTarget == target || bTarget == FE.GlobalAndLocalTarget then (
                        realToFrac $ AC.featureBassEnergyGridCoeff cSettings )
                     else 0.0 
  let gBaseOpacity = (AC.gridOpacity cSettings) / 100.0 :: GLfloat
      gOpacity     = gBaseOpacity + (lCoeff * loudness) + (bCoeff * basslevel)
      gMaterial    = AC.gridMaterial cSettings

  let sBaseOpacity = (AC.surfaceOpacity cSettings) / 100.0 :: GLfloat
      sOpacity     = sBaseOpacity + (lCoeff * loudness) + (bCoeff * basslevel)
      sMaterial    = AC.surfaceMaterial cSettings

  let newRadius    = realToFrac $ 0.3 + (lCoeff * loudness) + (bCoeff * basslevel) 

  let sigGen   = RH.signalGenerator rSettings
  let nSamples = SigGen.numSamples sigGen
  
  let visual = visualPrev { renderSettings  = rSettings, 
                            contextSettings = cSettings, 
                            gridMaterial    = gMaterial, 
                            surfaceMaterial = sMaterial,
                            gridOpacity     = gOpacity, 
                            surfaceOpacity  = sOpacity, 
                            radius          = newRadius, 
                            numSamples      = nSamples } 

  modifyIORef visualIORef ( \_ -> visual )
  return ()
-- }}}

tunnelRender :: IORef TunnelState -> IO ()
-- {{{
tunnelRender visualIORef = do 
  visual <- readIORef visualIORef
  return ()
-- }}}

