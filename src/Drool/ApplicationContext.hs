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
    Metrics(..), 
    MaterialConfig(..), 
    LightConfig(..), 
    defaultContextSettings, 
    saveContextSettings,
    saveContextSettingsAs, 
    loadContextSettings
) where

-- Moved ContextSettings to own module to solve ring dependency between Drool.Types
-- and Drool.SigGen.

import Debug.Trace

import Graphics.Rendering.OpenGL ( GLfloat, Color3(..), Color4(..), BlendingFactor(..), Capability(..) )
import Data.IORef
import qualified System.IO as SIO ( openFile, hPutStrLn, hGetLine, hClose, IOMode(..) ) 
import qualified Control.Concurrent as CC ( ThreadId ) 

import Drool.Types as DT
import Drool.Utils.SigGen as SigGen

import qualified Graphics.Rendering.OpenGL as GL ( BlendingFactor(..) ) 
import qualified Drool.Utils.Conversions as Conv ( blendModeSourceIndex, blendModeFrameBufferIndex )
import qualified Drool.Utils.FeatureExtraction as FE ( SignalFeaturesList(..), featureTargetIndex, FeatureTarget(..) ) 
import qualified Control.Concurrent.MVar as MV ( MVar )
import qualified Control.Concurrent.Chan as CC ( Chan )

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
                                         gridMaterial :: MaterialConfig, 
                                         surfaceMaterial :: MaterialConfig, 
                                         light0 :: LightConfig, 
                                         light1 :: LightConfig, 
                                         -- Scaling and opacity: 
                                         scaling :: Float,
                                         xLogScale :: GLfloat, 
                                         xLinScale :: GLfloat, 
                                         zLinScale :: GLfloat, 
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

                                         reverseBuffer :: Bool, 

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
                                         iirEnabled :: Bool, 
                                         ampEnabled :: Bool, 
                                      -- Sample rates
                                         numFFTBands :: Int, 
                                         iirCoef :: Float, 
                                         audioSampleRate :: Int, 
                                      -- Preferences
                                         -- Enable playback:
                                         playbackEnabled :: Bool, 
                                         -- Marquee text to render in visualizer: 
                                         marqueeText :: [Char], 
                                      -- Signal source options: 
                                         signalSource :: DT.SignalSource, 

                                         metrics :: Metrics }
  deriving ( Show, Read ) 

data MaterialConfig = MaterialConfig { materialAmbient :: Color4 GLfloat, 
                                       materialDiffuse :: Color4 GLfloat, 
                                       materialSpecular :: Color4 GLfloat, 
                                       materialEmission :: Color4 GLfloat, 
                                       materialShininess :: GLfloat }
  deriving ( Show, Read ) 

data LightConfig = LightConfig { lightIndex     :: Int, 
                                 lightIntensity :: GLfloat, 
                                 lightState     :: Capability, 
                                 lightAmbient   :: Color4 GLfloat, 
                                 lightDiffuse   :: Color4 GLfloat, 
                                 lightSpecular  :: Color4 GLfloat } 
  deriving ( Show, Read ) 
                             

data Metrics = Metrics { latency :: Integer } -- in ms
  deriving ( Show, Read ) 

defaultContextSettings :: ContextSettings
defaultContextSettings = ContextSettings { settingsFile = Nothing, 
                                           signalPushFrequency = 200, 
                                           renderingFrequency = 30, 
                                           signalBufferSize = 30,
                                           signalAmpDb = 1.0, 
                                           fixedRotation = DT.CRotationVector { DT.rotX = 0.0::GLfloat, DT.rotY = 0.0::GLfloat, DT.rotZ = 0.0::GLfloat },
                                           incRotation = DT.CRotationVector { DT.rotX = 0.0::GLfloat, DT.rotY = 0.0::GLfloat, DT.rotZ = 0.0::GLfloat },
                                           incRotationAccum = DT.CRotationVector { DT.rotX = 0.0::GLfloat, DT.rotY = 0.0::GLfloat, DT.rotZ = 0.0::GLfloat },
                                           scaling = 24,
                                           xLogScale = 15.0, 
                                           xLinScale = 5.0, 
                                           zLinScale = 5.0, 
                                           viewAngle = 121.0, 
                                           viewDistance = -2.1, 
                                           blendModeSourceIdx = Conv.blendModeSourceIndex SrcAlpha, 
                                           blendModeFrameBufferIdx = Conv.blendModeFrameBufferIndex OneMinusSrcAlpha, 
                                           useNormals = True, 
                                           normalsScale = 10.0, 
                                           rangeAmps = [ 0.56, 1.0, 1.33, 1.61, 1.66 ], 
                                           gridOpacity = 1.0,
                                           surfaceOpacity = 90.0,
                                           light0 = LightConfig { lightIndex     = 0, 
                                                                  lightState     = Enabled, 
                                                                  lightIntensity = 1.0,
                                                                  lightAmbient   = Color4 0.074 0.94 0.69 1.0, 
                                                                  lightDiffuse   = Color4 0.074 0.94 0.69 1.0, 
                                                                  lightSpecular  = Color4 0.074 0.94 0.69 1.0 }, 
                                           light1 = LightConfig { lightIndex     = 1, 
                                                                  lightState     = Enabled, 
                                                                  lightIntensity = 1.0, 
                                                                  lightAmbient   = Color4 0.074 0.94 0.69 1.0, 
                                                                  lightDiffuse   = Color4 0.074 0.94 0.69 1.0, 
                                                                  lightSpecular  = Color4 0.074 0.94 0.69 1.0 }, 
                                           surfaceMaterial = MaterialConfig { materialEmission  = Color4 0.0  0.0  0.0 0.0, 
                                                                              materialAmbient   = Color4 0.02 0.34 1.0 1.0, 
                                                                              materialDiffuse   = Color4 0.02 0.68 1.0 1.0, 
                                                                              materialSpecular  = Color4 0.48 1.0  1.0 1.0, 
                                                                              materialShininess = 30.0 },
                                           gridMaterial = MaterialConfig { materialEmission  = Color4 0.0  0.0  0.0  0.0, 
                                                                           materialAmbient   = Color4 0.03 0.04 0.01 1.0, 
                                                                           materialDiffuse   = Color4 0.11 0.16 0.08 1.0, 
                                                                           materialSpecular  = Color4 0.11 0.16 0.08 1.0, 
                                                                           materialShininess = 30.0 },
                                           renderPerspective = DT.Front,
                                           autoPerspectiveSwitch = False, 
                                           autoPerspectiveSwitchInterval = 500, 
                                           reverseBuffer = False, 
                                           maxBeatBand = 20, 
                                           iirEnabled = True, 
                                           fftEnabled = True, 
                                           ampEnabled = True, 
                                           playbackEnabled = True, 
                                           featureSignalEnergyTargetIdx = FE.featureTargetIndex FE.GlobalTarget, 
                                           featureBassEnergyTargetIdx = FE.featureTargetIndex FE.LocalTarget, 
                                           featureSignalEnergySurfaceCoeff = 0.2, 
                                           featureBassEnergySurfaceCoeff = 0.8, 
                                           featureSignalEnergyGridCoeff = 0.5, 
                                           featureBassEnergyGridCoeff = 0.5, 
                                           audioSampleRate = 191000, 
                                           numFFTBands = 10240, 
                                           iirCoef = 0.8, 
                                           marqueeText = "-D-R-O-O-L-", 
                                           signalSource = DT.Microphone, 
                                           metrics = Metrics { latency = 0 } }

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

instance Read Capability where
  readsPrec _ s  = tryParse [("Enabled", Enabled), ("Disabled", Disabled)]
    where tryParse [] = []
          tryParse ((attempt, result):xs) = if part == attempt 
                                            then [(result, remain)]
                                            else tryParse xs
            where part     = take (length attempt) stripped
                  remain   = drop (length attempt) stripped
                  stripped = (dropWhile (\c -> c == ' ') s)


