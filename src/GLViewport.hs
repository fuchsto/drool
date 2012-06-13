-----------------------------------------------------------------------------
--
-- Module      :  GLViewport
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

module GLViewport (
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

import Data.Maybe (fromMaybe)


display = do
  loadIdentity
  color (Color3 1 1 1 :: Color3 GLfloat)
  -- Instead of glBegin ... glEnd there is renderPrimitive.
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

reshape :: Maybe (Int, Int) -> IO ()
reshape dims = do
  let (width, height) = fromMaybe (canvasWidth, canvasHeight) dims
  viewport $= (Position 0 0, Size (fromIntegral width) (fromIntegral height))
  matrixMode $= Projection
  loadIdentity
  let (w, h) = if width <= height
                then (fromIntegral height, fromIntegral width )
                else (fromIntegral width,  fromIntegral height)
  perspective 60.0 (fromIntegral canvasWidth / fromIntegral canvasHeight) 1.0 20.0
  matrixMode $= Modelview 0
  loadIdentity

initComponent gtkBuilder = do
  putStrLn "Initializing OpenGL viewport"

  glConfig <- GtkGL.glConfigNew [GtkGL.GLModeRGBA, GtkGL.GLModeMultiSample,
                                 GtkGL.GLModeDouble, GtkGL.GLModeDepth, GtkGL.GLModeAlpha]
  GtkGL.initGL

  canvas <- GtkGL.glDrawingAreaNew glConfig

  -- Initialise some GL setting just before the canvas first gets shown
  -- (We can't initialise these things earlier since the GL resources that
  -- we are using wouldn't heve been setup yet)

  -- Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ -> do
  --   GL.drawBuffer $= GL.BackBuffers

  Gtk.onRealize canvas $ GtkGL.withGLDrawingArea canvas $ \_ -> do
    reconfigure canvasWidth canvasHeight
    return ()

  -- OnShow handler for GL canvas:
  Gtk.onExpose canvas $ \_ -> do
    GtkGL.withGLDrawingArea canvas $ \glwindow -> do
      GL.clear [GL.DepthBuffer, GL.ColorBuffer]
      display
      GtkGL.glDrawableSwapBuffers glwindow
    return True


  Gtk.onConfigure canvas $ \ (Gtk.Configure _ _ _ w h) -> do
    (w', h') <- reconfigure w h
    return True

  -- Resize handler:
  -- Gtk.onSizeAllocate canvas (reshape canvas)

  -- Add canvas (OpenGL drawing area) to GUI:
  Gtk.widgetSetSizeRequest canvas 400 300
  -- gtkViewport <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToViewport "glViewport"
  gtkViewport <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToHBox "glViewport"
  Gtk.widgetSetSizeRequest gtkViewport 400 300
  -- Gtk.containerAdd gtkViewport canvas
  Gtk.set gtkViewport [ Gtk.containerChild := canvas ]

  Gtk.containerResizeChildren gtkViewport

  Gtk.timeoutAddFull (do
      Gtk.widgetQueueDraw canvas
      return True)
    Gtk.priorityDefaultIdle 3

canvasWidth = 400
canvasHeight = 300
