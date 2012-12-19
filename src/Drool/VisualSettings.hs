-----------------------------------------------------------------------------
--
-- Module      :  Drool.VisualSettings
-- Copyright   :  Tobias Fuchs
-- License     :  MIT
--
-- Maintainer  :  twh.fuchs@gmail.com
-- Stability   :  experimental
-- Portability :  POSIX
--
-- |
--
-----------------------------------------------------------------------------

{-# OPTIONS -O2 -Wall #-}

module Drool.VisualSettings (
    VisualSettings(..), 
) where

import Drool.UI.Visuals.Visual
import Drool.UI.Visuals.FFTSurface
import Drool.UI.Visuals.Spheres
import Drool.UI.Visuals.Tunnel
import Drool.UI.Visuals.Blank


data VisualSetting = VisualSetting { spheresState :: SpheresState } | 
                     VisualSetting { fftSurfaceState :: FFTSurfaceState } | 
                     VisualSetting { tunnelState :: TunnelState } |
                     VisualSetting { blankState :: BlankState }
  deriving ( Read, Show )

data VisualSettings = { visualSettings :: [ VisualSetting ] } 


