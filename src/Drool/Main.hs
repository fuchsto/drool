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
import Data.Array.IO

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder

import qualified Drool.Types as DT
import qualified Drool.UI.ViewOptions as ViewOptions
import qualified Drool.UI.GLWindow as GLWindow



main = do
  Gtk.initGUI

  emptyBuffer <- DT.newSignalBuffer
  contextSettings <- newIORef(
    DT.ContextSettings { DT.translation = undefined,
                         DT.rotation = (0.0::GLfloat, 0.0::GLfloat, 0.0::GLfloat),
                         DT.angle = (0.0::GLfloat),
                         DT.scaling = 0.5,
                         DT.gridOpacity = 0.8,
                         DT.surfaceOpacity = 0.3,
                         DT.renderPerspective = DT.Isometric,
                         DT.signalBuf = emptyBuffer } )

  -- samples as list
  let samples = [ sin (x/50) | x <- [0..512] ] :: [GLfloat]
  -- let samples = [ 0..10 ]
  let buildSamples phase = [ sin (x/phase) | x <- [0..512] ] :: [GLfloat]

  -- samples to array
  signal <- (newListArray (0, length samples - 1) samples)::IO (IOArray Int GLfloat)
  -- Construct signal buffer with 10 signals (10 times the same signal)
  signals <- ((newArray(0,9) ((DT.CSignal signal)::DT.Signal))::IO (IOArray Int DT.Signal))
  let buf = (DT.CSignalBuffer signals)::DT.SignalBuffer

  settings <- readIORef contextSettings
  contextSettings $=! settings { DT.signalBuf = buf }

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

  -- Redraw canvas every 3ms:
  updateSettingsTimer <- Gtk.timeoutAddFull (do
      settings <- get contextSettings
      -- Update context settings:
      contextSettings $=! settings { DT.angle = (DT.angle settings)+1 }
      return True)
    Gtk.priorityDefaultIdle 3

  -- Remove timer for redrawing canvas when closing window:
  Gtk.onDestroy mainWindow (Gtk.timeoutRemove updateSettingsTimer)

  -- Display window:
  Gtk.widgetShowAll mainWindow

  -- Enter GUI main loop:
  Gtk.mainGUI
