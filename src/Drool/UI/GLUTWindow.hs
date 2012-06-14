-----------------------------------------------------------------------------
--
-- Module      :  GLUTWindow
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

module Drool.UI.GLUTWindow (
  initComponent
) where

import Data.IORef

import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT
import Graphics.UI.GLUT.Callbacks.Window
import Graphics.UI.GLUT.Callbacks.Global

import qualified Drool.Types as DT



initComponent builder signalBuf settings = do
  putStrLn "Initializing component GLUTWindow"

  initialDisplayMode $= [WithDepthBuffer,DoubleBuffered]

  putStrLn "Opening GLUT window"
  createWindow "drool visualizer"

  reshapeCallback $= Just reshape
  idleCallback $= Just (idle signalBuf)
  displayCallback $= (display signalBuf settings)

  mainLoop


display signalBuf settingsIORef = do

  GL.clear [ColorBuffer, DepthBuffer]
  GL.loadIdentity
{-
  let tSettings = DT.translation settings
  let rSettings = DT.rotation settings
-}

  signal <- get signalBuf -- IORef to list of fourier transformations.

{-
  translation <- get tSettings :: DT.TVector
  rotation <- get rSettings :: DT.RVector
-}

  settings <- get settingsIORef

  let translation = DT.translation settings
  let rotation = DT.rotation settings

  -- GL.translate $ translation

  GL.rotate ((DT.rvector_x rotation)::GL.GLfloat) $ Vector3 (1::GL.GLfloat) (0::GL.GLfloat) (0::GL.GLfloat)
  GL.rotate ((DT.rvector_y rotation)::GL.GLfloat) $ Vector3 (0::GL.GLfloat) (1::GL.GLfloat) (0::GL.GLfloat)
  GL.rotate ((DT.rvector_z rotation)::GL.GLfloat) $ Vector3 (0::GL.GLfloat) (0::GL.GLfloat) (1::GL.GLfloat)

  swapBuffers

idle signalBuf = do
  postRedisplay Nothing

reshape s@(Size w h) = do
  putStrLn "GLUTWindow.reshape"
  viewport $= (Position 0 0, s)
