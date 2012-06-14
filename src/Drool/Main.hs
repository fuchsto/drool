-----------------------------------------------------------------------------
--
-- Module      :  Main
-- Copyright   :
-- License     :  GPL v3
--
-- Maintainer  :  Tobias Fuchs
-- Stability   :  unstable
-- Portability :  Win32, POSIX
--
-- |
--
-----------------------------------------------------------------------------

-- Main controller of the application, managing IORefs between ViewOptions
-- and rendering display.

module Main where

import Debug.Trace

import Data.IORef

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder

import qualified Drool.Types as DT
import qualified Drool.UI.ViewOptions as ViewOptions
import qualified Drool.UI.GLWindow as GLWindow



main = do
  Gtk.initGUI

-- IORef to list of fourier transformations.
  signalBuf <- newIORef(DT.newSignalBuffer)

  contextSettings <- newIORef(
    DT.ContextSettings { DT.translation = undefined,
                         DT.rotation = (0.0::GLfloat, 0.0::GLfloat, 0.0::GLfloat),
                         DT.angle = (0.0::GLfloat) } )


  -- Load UI configuration from GtkBuilder file:
  builder <- GtkBuilder.builderNew
  GtkBuilder.builderAddFromFile builder "droolui-test.glade"

  -- Instatiate window from GtkBuilder file:
  mainWindow <- GtkBuilder.builderGetObject builder Gtk.castToWindow "mainWindow"


  openVisualizerButton <- GtkBuilder.builderGetObject builder Gtk.castToButton "buttonVisualizer"
  Gtk.onClicked openVisualizerButton $ do
    GLWindow.initComponent builder contextSettings

  -- Application exit callback (quits main loop):
  Gtk.onDestroy mainWindow Gtk.mainQuit

  -- Define button callbacks:
  menuItem_FileClose <- GtkBuilder.builderGetObject builder Gtk.castToMenuItem "menuitemFileQuit"
  Gtk.on menuItem_FileClose Gtk.menuItemActivate $ do
    putStrLn "Exiting"
    Gtk.widgetDestroy mainWindow

  ViewOptions.initComponent builder contextSettings

  -- Display window:
  Gtk.widgetShowAll mainWindow

  -- Enter GUI main loop:
  Gtk.mainGUI
