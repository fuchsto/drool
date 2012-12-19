-----------------------------------------------------------------------------
--
-- Module      :  Drool.UI.Visuals
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

module Drool.UI.Visuals (
    module Drool.UI.Visuals.Visual, 
    module Drool.UI.Visuals.FFTSurface, 
    module Drool.UI.Visuals.Spheres,
    module Drool.UI.Visuals.Tunnel,
    module Drool.UI.Visuals.Blank, 
    VisualModel(..), 
    visualModelIndex, 
    visualModelFromIndex, 
    newVisual
) where

import Data.IORef ( IORef, newIORef )

import Drool.Utils.RenderHelpers ( RenderSettings )

import Drool.UI.Visuals.Visual
import Drool.UI.Visuals.FFTSurface
import Drool.UI.Visuals.Spheres
import Drool.UI.Visuals.Tunnel
import Drool.UI.Visuals.Blank
import Drool.UI.Visuals.Disco

import Drool.ApplicationContext as AC ( ContextSettings ) 

data VisualModel = BlankModel | SpheresModel | FFTSurfaceModel | TunnelModel
  deriving ( Read, Show, Eq )

visualModelIndex :: VisualModel -> Int
visualModelIndex vm = case vm of 
  BlankModel      -> 0
  SpheresModel    -> 1
  FFTSurfaceModel -> 2
  TunnelModel     -> 3

visualModelFromIndex :: Int -> VisualModel
visualModelFromIndex idx = case idx of 
  0 -> BlankModel
  1 -> SpheresModel
  2 -> FFTSurfaceModel
  3 -> TunnelModel
  _ -> BlankModel

newVisual :: VisualModel -> IORef AC.ContextSettings -> IO Visual
newVisual SpheresModel contextSettings = do 
  visualInitState      <- newSpheresState contextSettings 
  visualInitStateIORef <- newIORef visualInitState
  return $ newSpheresVisual contextSettings visualInitStateIORef
newVisual FFTSurfaceModel contextSettings = do 
  visualInitState      <- newFFTSurfaceState contextSettings 
  visualInitStateIORef <- newIORef visualInitState
  return $ newFFTSurfaceVisual contextSettings visualInitStateIORef
newVisual TunnelModel contextSettings = do 
  visualInitState      <- newTunnelState contextSettings 
  visualInitStateIORef <- newIORef visualInitState
  return $ newTunnelVisual contextSettings visualInitStateIORef
newVisual BlankModel _ = do 
  return $ newBlankVisual 

{-
loadVisual :: VisualSettings -> IORef AC.ContextSettings -> IO Visual
loadVisual state@(VisualSettings SpheresState) contextSettings = do
  stateIORef <- newIORef spheresState state
  return newSpheresVisual contextSettings stateIORef
loadVisual state@(VisualSettings FFTSurfaceState) contextSettings = do
  stateIORef <- newIORef fftSurfaceState state
  return newFFTSurfaceVisual contextSettings stateIORef
loadVisual state@(VisualSettings TunnelState) contextSettings = do
  stateIORef <- newIORef tunnelState state
  return newTunnelVisual contextSettings stateIORef
loadVisual state@(VisualSettings BlankState) contextSettings = do
  return newBlankVisual 
-}

