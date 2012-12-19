-----------------------------------------------------------------------------
--
-- Module      :  Drool.UI.Visuals.Blank
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

module Drool.UI.Visuals.Blank (
    BlankState, 
    newBlankVisual
) where

-- Imports
-- {{{
import Data.IORef ( IORef, readIORef )

import Drool.UI.Visuals.Visual as Visual
import qualified Drool.ApplicationContext as AC ( ContextSettings(..) ) 
import qualified Drool.Utils.RenderHelpers as RH ( RenderSettings )
import Graphics.Rendering.OpenGL ( GLfloat )
-- }}}

data BlankState = BlankState { contextSettings :: AC.ContextSettings, 
                               renderSettings :: RH.RenderSettings }

instance VState BlankState where 
  vsRenderSettings = renderSettings

newBlankState :: IORef AC.ContextSettings -> IO BlankState
newBlankState cSettingsIORef = do 
  cSettings <- readIORef cSettingsIORef
  let settings = BlankState { contextSettings = cSettings } 
  return settings

-- Hook Visual state IORef to concrete implementations: 
newBlankVisual :: Visual
newBlankVisual = Visual { dimensions = noneDimensions, 
                          update     = noneUpdate, 
                          render     = noneRender }


noneDimensions :: IO (GLfloat,GLfloat,GLfloat)
noneDimensions = do
  return (1.0,1.0,1.0)

noneUpdate :: RH.RenderSettings -> Int -> IO ()
noneUpdate _ _ = do
  return ()

noneRender :: IO ()
noneRender = do 
  return ()

