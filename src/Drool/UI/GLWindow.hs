-----------------------------------------------------------------------------
--
-- Module      :  Drool.UI.GLWindow
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

module Drool.UI.GLWindow (
    initComponent
) where


import Control.Monad (unless)
import Data.List (stripPrefix)
import System.Exit (exitFailure)
import Data.IORef
import Data.List
import Graphics.Rendering.OpenGL as GL
import qualified Graphics.UI.Gtk as Gtk
import Graphics.UI.Gtk.Abstract.Widget as GtkAbstractWidget
import Graphics.UI.Gtk.Builder as GtkBuilder
import qualified Graphics.UI.Gtk.OpenGL as GtkGL
import System.Random

import Graphics.UI.Gtk (AttrOp((:=)))

import qualified Drool.Types as DT

import Data.Maybe (fromMaybe)


display contextSettings = do
  loadIdentity
  color (Color3 1 1 1 :: Color3 GLfloat)

  settings <- get contextSettings
  let angle = DT.angle settings

  rotate angle $ Vector3 0 1.0 0

  renderPrimitive Polygon $ do
    vertex (Vertex3 0.25 0.25 0.0 :: Vertex3 GLfloat)
    vertex (Vertex3 0.75 0.25 0.0 :: Vertex3 GLfloat)
    vertex (Vertex3 0.75 0.75 0.0 :: Vertex3 GLfloat)
    vertex (Vertex3 0.25 0.75 0.0 :: Vertex3 GLfloat)

reconfigure :: Int -> Int -> IO (Int, Int)
reconfigure w h = do
  -- maintain aspect ratio
  let aspectRatio = (fromIntegral canvasWidth) / (fromIntegral canvasHeight)
      (w1, h1)    = (fromIntegral w, (fromIntegral w) / aspectRatio)
      (w2, h2)    = ((fromIntegral h) * aspectRatio, fromIntegral h)
      (w', h')    = if h1 <= fromIntegral h
                      then (floor w1, floor h1)
                      else (floor w2, floor h2)
  reshape $ Just (w', h')
  return (w', h')

reshape allocation = do
  let width  = 400
  let height = 400
  viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
  matrixMode $= Modelview 0
  return ()

initComponent gtkBuilder contextSettings = do

  window <- Gtk.windowNew

  Gtk.set window [ Gtk.containerBorderWidth := 8,
                   Gtk.windowTitle := "drool visualizer" ]

  putStrLn "Initializing OpenGL viewport"

  glConfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA, GtkGL.GLModeMultiSample,
                                 GtkGL.GLModeDouble, GtkGL.GLModeDepth, GtkGL.GLModeAlpha]
  GtkGL.initGL

  canvas <- GtkGL.glDrawingAreaNew glConfig

  -- Initialise some GL setting just before the canvas first gets shown
  -- (We can't initialise these things earlier since the GL resources that
  -- we are using wouldn't heve been setup yet)

  Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ -> do
    reconfigure canvasWidth canvasHeight
    return ()

  -- OnShow handler for GL canvas:
  Gtk.onExpose canvas $ \_ -> do
    GtkGL.withGLDrawingArea canvas $ \glwindow -> do
      GL.clear [GL.DepthBuffer, GL.ColorBuffer]
      display contextSettings
      GtkGL.glDrawableSwapBuffers glwindow
    return True

  -- Resize handler:
  Gtk.onSizeAllocate canvas (reshape)

  -- Add canvas (OpenGL drawing area) to GUI:
  Gtk.widgetSetSizeRequest canvas canvasWidth canvasHeight

  Gtk.set window [ Gtk.containerChild := canvas ]

  -- Redraw canvas every 3ms:
  updateCanvasTimer <- Gtk.timeoutAddFull (do
      Gtk.widgetQueueDraw canvas
      return True)
    Gtk.priorityDefaultIdle 3

  -- Remove timer for redrawing canvas when closing window:
  Gtk.onDestroy window (Gtk.timeoutRemove updateCanvasTimer)

  Gtk.widgetShowAll window

canvasWidth = 400
canvasHeight = 400

