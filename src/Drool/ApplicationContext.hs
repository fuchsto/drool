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
    ContextSettings(..)
) where

-- Moved ContextSettings to own module to solve ring dependency between Drool.Types
-- and Drool.SigGen.

import Graphics.Rendering.OpenGL
import Data.IORef
import qualified Control.Concurrent as CC ( ThreadId ) 

import Drool.Types as DT
import Drool.Utils.SigGen as SigGen


-- Shared settings for communication between main controller, view options
-- and rendering:
data ContextSettings = ContextSettings { samplingThreadId :: CC.ThreadId, -- Thread ID of sampling thread
                                         signalBuf :: (IORef SignalList),
                                         
                                      -- Signal Buffer Options: 
                                         signalPushFrequency :: Int,  -- Maximum frequency signals are pushed to rendering 
                                         renderingFrequency :: Int, -- Maximum frequency of GL rendering loop
                                         signalBufferSize :: Int,   -- Size of signal buffer 
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
                                         -- Vector stuff: 
                                         useNormals :: Bool, 
                                         normalsScale :: Float, 
                                         -- Perspective: 
                                         renderPerspective :: RenderPerspective,
                                      -- Feature extraction: 
                                         maxBeatBandSamples :: Int, 
                                      -- Transformation options: 
                                         fftEnabled :: Bool, 
                                         numFFTBands :: Int, 
                                      -- Preferences
                                         -- Enable playback:
                                         playbackEnabled :: Bool, 
                                      -- Signal source options: 
                                         signalSource :: DT.SignalSource, 
                                         signalGenerator :: (SigGen.SignalGenerator) }

