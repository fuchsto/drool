-----------------------------------------------------------------------------
--
-- Module      :  Drool.VisualConfig
-- Copyright   :  2012, Tobias Fuchs
-- License     :  MIT
--
-- Maintainer  :  twh.fuchs@gmail.com
-- Stability   :  experimental
-- Portability :  POSIX
--
-- |
--
-----------------------------------------------------------------------------

module Drool.VisualConfig (
    VisualConfig
) where 

data VisualConfig = VisualConfig { incRotation :: RotationVector,      -- Incremental rotation step size
                                   incRotationAccum :: RotationVector, -- Incremental rotation accumulated value (sum of step sizes)
                                   fixedRotation :: RotationVector,    -- Fixed rotation vector
                                   rotationUseGlobal :: Bool, 
                                   -- Light: 
                                   light0 :: LightConfig, 
                                   light1 :: LightConfig, 
                                   lightUseGlobal :: Bool, 
                                   -- Colors: 
                                   gridMaterial :: MaterialConfig, 
                                   surfaceMaterial :: MaterialConfig, 
                                   gridOpacity :: GLfloat,
                                   surfaceOpacity :: GLfloat,
                                   -- Perspective: 
                                   renderPerspective :: RenderPerspective,
                                   autoPerspectiveSwitch :: Bool, 
                                   autoPerspectiveSwitchInterval :: Int, 
                                   perspectiveUseGlobal :: Bool, 
                                   -- View: 
                                   viewAngle :: GLfloat, 
                                   viewDistance :: GLfloat, 
                                   -- Blending: 
                                   blendModeSourceIdx :: Int, 
                                   blendModeFrameBufferIdx :: Int }
