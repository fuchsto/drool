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
                                           signalAmpDb = 1.0, 
                                           fixedRotation = DT.CRotationVector { DT.rotX = 0.0::GLfloat, DT.rotY = 0.0::GLfloat, DT.rotZ = 0.0::GLfloat },
                                           incRotation = DT.CRotationVector { DT.rotX = 0.0::GLfloat, DT.rotY = 0.0::GLfloat, DT.rotZ = 0.0::GLfloat },
                                           incRotationAccum = DT.CRotationVector { DT.rotX = 0.0::GLfloat, DT.rotY = 0.0::GLfloat, DT.rotZ = 0.0::GLfloat },
                                           scaling = 20,
                                           blendModeSourceIdx = Conv.blendModeSourceIndex SrcAlpha, 
                                           blendModeFrameBufferIdx = Conv.blendModeFrameBufferIndex DstAlpha, 
                                           useNormals = True, 
                                           normalsScale = 10.0, 
                                           rangeAmps = [ 1.0, 1.0, 1.0, 1.0, 1.0 ], 
                                           gridOpacity = 15,
                                           surfaceOpacity = 13,
                                           surfaceColor = Color3 (62.0/255) (187.0/255) (1::GLfloat),
                                           lightColor = Color3 (239.0/255) (19.0/255) (19.0/255.0::GLfloat) ,
                                           gridColor = Color3 (142.0/255) 1 (58.0/255::GLfloat),
                                           renderPerspective = DT.Isometric,
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
                                           playbackEnabled = False, 
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


