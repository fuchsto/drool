-----------------------------------------------------------------------------
--
-- Module      :  Drool.UI.Visuals.Visual
-- Copyright   :  Tobias Fuchs
-- License     :  AllRightsReserved
--
-- Maintainer  :  twh.fuchs@gmail.com
-- Stability   :  experimental
-- Portability :  POSIX
--
-- |
--
-----------------------------------------------------------------------------

{-# OPTIONS -O2 -Wall #-}

module Drool.UI.Visuals.Visual (
    Visual(..), 
    VState(..)
) where

import Data.IORef
import Graphics.Rendering.OpenGL ( GLfloat )
import Drool.Utils.RenderHelpers ( RenderSettings )

class VState vs where 
  vsRenderSettings :: vs -> RenderSettings

data Visual = Visual { dimensions :: IO (GLfloat,GLfloat,GLfloat), 
                       update :: RenderSettings -> Int -> IO (), 
                       render :: IO () }

