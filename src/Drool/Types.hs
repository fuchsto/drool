-----------------------------------------------------------------------------
--
-- Module      :  Drool.Types
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

module Drool.Types (
   RotationVector,
   ContextSettings(..),
   Signal,
   SignalBuffer,
   rvector_x, rvector_y, rvector_z,
   newSignalBuffer
) where

import Data.Array.IO
import Graphics.Rendering.OpenGL
import Data.IORef

newtype Signal = CSignal (IOArray Int GLfloat)
-- An array of signal[time] = [ GLFloat, ... ]
-- In effect, a two-dimensional matrix.
newtype SignalBuffer = CSignalBuffer (IOArray Int Signal)

-- Type for translation vector
-- newtype TVector = TVector (Vector3 GLfloat GLfloat GLfloat)
-- Type for rotation vector
type RotationVector = (GLfloat, GLfloat, GLfloat)

-- Shared settings for communication between main controller, view options
-- and rendering:
data ContextSettings = ContextSettings { translation :: Vector3(GLfloat,GLfloat,GLfloat),
                                         rotation :: RotationVector,
                                         angle :: GLfloat }

rvector_x :: (a,a,a) -> a
rvector_x (x,y,z) = x

rvector_y :: (a,a,a) -> a
rvector_y (x,y,z) = y

rvector_z :: (a,a,a) -> a
rvector_z (x,y,z) = z

newSignal :: IO Signal
newSignal = fmap CSignal $ newArray(1,1024) (0::GLfloat) :: IO Signal

newSignalBuffer = do
  blankSignal <- newSignal
  fmap CSignalBuffer $ newArray(1,100) (blankSignal) :: IO SignalBuffer
