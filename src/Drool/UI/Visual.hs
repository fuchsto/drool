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

module Drool.UI.Visual (
    Visual(..)
) where

import Data.IORef (IORef)
import Graphics.Rendering.OpenGL ( GLfloat )
import Drool.ApplicationContext ( ContextSettings, ContextObjects )
import Drool.Utils.RenderHelpers ( RenderSettings )

class Visual v where 
  newVisual :: ContextSettings -> ContextObjects -> RenderSettings -> IO (v)
  dimensions :: v -> (GLfloat,GLfloat,GLfloat)
  pushSignal :: IORef v -> ContextSettings -> RenderSettings -> Int -> IO (v)
  render :: v -> IO ()


  
  
