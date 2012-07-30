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

module Drool.UI.Visuals (
    VisualComponent(..), 
    module Drool.UI.Visuals.Visual, 
    module Drool.UI.Visuals.FFTSurface, 
    module Drool.UI.Visuals.Spheres
) where

import Drool.UI.Visuals.Visual
import Drool.UI.Visuals.FFTSurface
import Drool.UI.Visuals.Spheres

data VisualComponent = FFTSurfaceVisual FFTSurface | SpheresVisual Spheres

{- 
Function record approach using currying. This effectively achieves the same 
interface behaviour, but without the Visual type class. 
Currently, it is: 

  class Visual v where 
    newVisual :: ContextSettings -> ContextObjects -> RenderSettings -> IO (v)
    dimensions :: v -> (GLfloat,GLfloat,GLfloat)
    pushSignal :: IORef v -> ContextSettings -> RenderSettings -> Int -> IO (v)
    render :: v -> IO ()

Using a record of functions: 

  data Visual = Visual { newVisual :: RenderSettings -> IO (v)
                         dimensions :: (GLfloat,GLfloat,GLfloat)
                         pushSignal :: RenderSettings -> Int -> IO (v)
                         render :: IO () }

and in the implemenation modules, e.g. FFTSurface: 
  NOTE: contextSettings and contextObjects have to be passed as IORefs in this case. 

  makeVisual :: Visual
  makeVisual contextSettingsIORef contextObjectsIORef = Visual { newVisual  = fftSurfaceNewVisual contextSettingsIORef contextObjectsIORef, -- curried: renderSettings -> IO (v) 
                                                                 dimensions = fftSurfaceDimensions, 
                                                                 pushSignal = fftSurfacePushSignal contextSettingsIORef -- curried: renderSettings -> IORef v -> IO (v)
                                                                 render     = IO () -- curried: v } 
in Main: 

  let visualRec = FFTSurface.makeVisual contextSettings contextObjects 
  -- IORefs contextSettings and contextObjects are bound to record now
  -- Interface implementation is bound in makeVisual. 
  visualRecIORef <- newIORef visualRec

in GLWindow: 

  visualRec <- readIORef visualRecIORef
  updatedVisualRec <- (pushSignal visualRec) renderSettings visualRecIORef

-}
