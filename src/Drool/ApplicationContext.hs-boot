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

import Graphics.Rendering.OpenGL ( GLfloat, Color3(..), Color4(..), BlendingFactor(..) )
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
																				 light0Enabled :: Bool, 
                                         light0 :: LightConfig, 
																				 light1Enabled :: Bool, 
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

data MaterialConfig = MaterialConfig { materialAmbient :: Color4 GLfloat, 
                                       materialDiffuse :: Color4 GLfloat, 
                                       materialSpecular :: Color4 GLfloat, 
                                       materialEmission :: Color4 GLfloat, 
                                       materialShininess :: GLfloat }

data LightConfig = LightConfig { lightAmbient :: Color4 GLfloat, 
                                 lightDiffuse :: Color4 GLfloat, 
                                 lightSpecular :: Color4 GLfloat } 

data Metrics = Metrics { latency :: Integer } -- in ms

defaultContextSettings :: ContextSettings

-- Non-serializable context settings. 
data ContextObjects = ContextObjects { samplingThreadId :: CC.ThreadId, 
                                       samplingSem :: MV.MVar Int, 
                                       numNewSignalsChan :: CC.Chan Int, 
                                       renderingSem :: MV.MVar Int, 
                                       signalBuf :: (IORef SignalList), 
                                       featuresBuf :: (IORef FE.SignalFeaturesList), 
                                       signalGenerator :: (SigGen.SignalGenerator) } 

saveContextSettingsAs :: ContextSettings -> String -> IO Bool

saveContextSettings :: ContextSettings -> IO Bool

loadContextSettings :: String -> IO ContextSettings 

