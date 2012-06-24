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
data ContextSettings = ContextSettings { samplingFrequency :: Int,
                                         renderingFrequency :: Int,
                                         signalBufferSize :: Int,

                                         translation :: Vector3(GLfloat,GLfloat,GLfloat),
                                         rotation :: RotationVector,
                                         angle :: GLfloat,

                                         gridColor :: Color3 GLfloat,
                                         surfaceColor :: Color3 GLfloat,
                                         lightColor :: Color3 GLfloat,

                                         scaling :: GLfloat,
                                         gridOpacity :: GLfloat,
                                         surfaceOpacity :: GLfloat,

                                         renderPerspective :: RenderPerspective,

                                         signalBuf :: (IORef SignalList),
                                         signalGenerator :: (SigGen.SignalGenerator) }
