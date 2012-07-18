-----------------------------------------------------------------------------
--
-- Module      :  Drool.ApplicationContext
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

module Drool.ApplicationContext (
    ContextSettings(..),
    ContextObjects(..), 
    defaultContextSettings, 
    saveContextSettings,
    saveContextSettingsAs, 
    loadContextSettings
) where

-- Moved ContextSettings to own module to solve ring dependency between Drool.Types
-- and Drool.SigGen.

import Graphics.Rendering.OpenGL
import Data.IORef
import qualified System.IO as SIO ( openFile, hPutStrLn, hGetLine, hClose, IOMode(..) ) 
import qualified Control.Concurrent as CC ( ThreadId ) 

import Drool.Types as DT
import Drool.Utils.SigGen as SigGen

import qualified Graphics.Rendering.OpenGL as GL ( BlendingFactor(..) ) 
import qualified Drool.Utils.Conversions as Conv ( blendModeSourceIndex, blendModeFrameBufferIndex )
import qualified Drool.Utils.FeatureExtraction as FE ( SignalFeaturesList(..), featureTargetIndex, FeatureTarget(..) ) 

-- Shared settings for communication between main controller, view options
-- and rendering. 
-- This record is serializable so it can be saved to a settings file. 
data ContextSettings = ContextSettings { settingsFile :: Maybe String, 
                                         
                                      -- Signal Buffer Options: 
                                         signalPushFrequency :: Int,  -- Maximum frequency signals are pushed to rendering 
                                         renderingFrequency :: Int, -- Maximum frequency of GL rendering loop
                                         signalBufferSize :: Int,   -- Size of signal buffer 

                                         signalAmpDb :: Float, 
                                      -- View Options: 
                                         incRotation :: RotationVector,      -- Incremental rotation step size
                                         incRotationAccum :: RotationVector, -- Incremental rotation accumulated value (sum of step sizes)
                                         fixedRotation :: RotationVector,    -- Fixed rotation vector
                                         -- Colors: 
                                         gridColor :: Color3 GLfloat,
                                         surfaceColor :: Color3 GLfloat,
                                         lightColor :: Color3 GLfloat,
                                         -- Scaling and opacity: 
                                         scaling :: GLfloat,
                                         rangeAmps :: [Float], 
                                         gridOpacity :: GLfloat,
                                         surfaceOpacity :: GLfloat,
                                         -- View perspective
                                         viewAngle :: GLfloat, 
                                         viewDistance :: GLfloat, 
                                         -- Blending: 
                                         blendModeSourceIdx :: Int, 
                                         blendModeFrameBufferIdx :: Int,
                                         -- Feature Influence: 
                                         -- Target of signal energy feature (none, local, global): 
                                         featureSignalEnergyTargetIdx :: Int, 
                                         -- Target of bass energy feature (none, local, global): 
                                         featureBassEnergyTargetIdx :: Int, 
                                         -- Feature application coeffs for surface: 
                                         featureSignalEnergySurfaceCoeff :: Float, 
                                         featureBassEnergySurfaceCoeff :: Float, 
                                         -- Feature application coeffs for grid: 
                                         featureSignalEnergyGridCoeff :: Float, 
                                         featureBassEnergyGridCoeff :: Float, 
                                         -- Vector stuff: 
                                         useNormals :: Bool, 
                                         normalsScale :: Float, 
                                         -- Perspective: 
                                         renderPerspective :: RenderPerspective,
                                         autoPerspectiveSwitch :: Bool, 
                                         autoPerspectiveSwitchInterval :: Int, 
                                      -- Feature extraction: 
                                         maxBeatBand :: Int, 
                                      -- Transformation options: 
                                         fftEnabled :: Bool, 
                                         ampEnabled :: Bool, 
                                      -- Sample rates
                                         numFFTBands :: Int, 
                                         audioSampleRate :: Int, 
                                      -- Preferences
                                         -- Enable playback:
                                         playbackEnabled :: Bool, 
                                         -- Marquee text to render in visualizer: 
                                         marqueeText :: [Char], 
                                      -- Signal source options: 
                                         signalSource :: DT.SignalSource }
  deriving ( Show, Read ) 

defaultContextSettings :: ContextSettings
defaultContextSettings = ContextSettings { settingsFile = Nothing, 
                                           signalPushFrequency = 100, -- 200,
                                           renderingFrequency = 100, -- 150,
                                           signalBufferSize = 30,
                                           signalAmpDb = 5.0, 
                                           fixedRotation = DT.CRotationVector { DT.rotX = 0.0::GLfloat, DT.rotY = 0.0::GLfloat, DT.rotZ = 0.0::GLfloat },
                                           incRotation = DT.CRotationVector { DT.rotX = 0.0::GLfloat, DT.rotY = 0.0::GLfloat, DT.rotZ = 0.0::GLfloat },
                                           incRotationAccum = DT.CRotationVector { DT.rotX = 0.0::GLfloat, DT.rotY = 0.0::GLfloat, DT.rotZ = 0.0::GLfloat },
                                           scaling = 20,
                                           viewAngle = 121.0, 
                                           viewDistance = -2.1, 
                                           blendModeSourceIdx = Conv.blendModeSourceIndex SrcAlpha, 
                                           blendModeFrameBufferIdx = Conv.blendModeFrameBufferIndex DstAlpha, 
                                           useNormals = True, 
                                           normalsScale = 10.0, 
                                           rangeAmps = [ 1.0, 1.0, 1.0, 1.0, 1.0 ], 
                                           gridOpacity = 1.0,
                                           surfaceOpacity = 90.0,
                                           surfaceColor = Color3 0.24 0.68 1.0, 
                                           lightColor = Color3 0.074 0.94 0.69, 
                                           gridColor = Color3 0.11 0.16 0.08, 
                                           renderPerspective = DT.Front,
                                           autoPerspectiveSwitch = False, 
                                           autoPerspectiveSwitchInterval = 500, 
                                           maxBeatBand = 20, 
                                           fftEnabled = True, 
                                           ampEnabled = True, 
                                           featureSignalEnergyTargetIdx = FE.featureTargetIndex FE.GlobalTarget, 
                                           featureBassEnergyTargetIdx = FE.featureTargetIndex FE.LocalTarget, 
                                           featureSignalEnergySurfaceCoeff = 0.2, 
                                           featureBassEnergySurfaceCoeff = 0.8, 
                                           featureSignalEnergyGridCoeff = 0.5, 
                                           featureBassEnergyGridCoeff = 0.5, 
                                           audioSampleRate = 191000, 
                                           numFFTBands = 10240, 
                                           playbackEnabled = True, 
                                           marqueeText = "bass macht glücklich", 
                                           signalSource = DT.Microphone }

-- Non-serializable context settings. 
data ContextObjects = ContextObjects { samplingThreadId :: CC.ThreadId, 
                                       signalBuf :: (IORef SignalList), 
                                       featuresBuf :: (IORef FE.SignalFeaturesList), 
                                       signalGenerator :: (SigGen.SignalGenerator) } 

saveContextSettingsAs :: ContextSettings -> String -> IO Bool
saveContextSettingsAs settings filepath = saveContextSettings (settings { settingsFile = Just filepath })

saveContextSettings :: ContextSettings -> IO Bool
saveContextSettings settings = do
  case settingsFile settings of 
    Just filepath -> do 
      let serSettings = show settings
      handle <- SIO.openFile filepath SIO.WriteMode
      SIO.hPutStrLn handle serSettings
      SIO.hClose handle
      return True
    Nothing -> do 
      putStrLn "Cannot save settings without settings file path"
      return False

loadContextSettings :: String -> IO ContextSettings 
loadContextSettings filepath = do 
  handle <- SIO.openFile filepath SIO.ReadMode
  serSettings <- SIO.hGetLine handle
  let settings = read serSettings :: ContextSettings
  return settings


