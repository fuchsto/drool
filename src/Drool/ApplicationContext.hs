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

import Drool.Types as DT
import Drool.Utils.SigGen as SigGen


-- Shared settings for communication between main controller, view options
-- and rendering:
data ContextSettings = ContextSettings { samplingFrequency :: Int,  -- Maximum frequency of sampling loop
                                         renderingFrequency :: Int, -- Maximum frequency of GL rendering loop
                                         signalBufferSize :: Int,   -- Size of signal buffer 
                                         -- View Options: 
                                         translation :: Vector3(GLfloat,GLfloat,GLfloat), -- 
                                         incRotation :: RotationVector,      -- Incremental rotation step size
                                         incRotationAccum :: RotationVector, -- Incremental rotation accumulated value (sum of step sizes)
                                         fixedRotation :: RotationVector,    -- Fixed rotation vector
                                          
                                         gridColor :: Color3 GLfloat,
                                         surfaceColor :: Color3 GLfloat,
                                         lightColor :: Color3 GLfloat,

                                         scaling :: GLfloat,
                                         gridOpacity :: GLfloat,
                                         surfaceOpacity :: GLfloat,
                                         
                                         renderPerspective :: RenderPerspective,
                                         -- Feature extraction: 
                                         maxBeatBandSamples :: Int, 
                                         -- Signal source settings: 
                                         signalBuf :: (IORef SignalList),
                                         signalGenerator :: (SigGen.SignalGenerator) }
