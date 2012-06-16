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

{-# OPTIONS -O2 -Wall #-}

module Main where

import Data.IORef
import Data.Array.IO

import Graphics.Rendering.OpenGL
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder

import qualified Drool.Types as DT
import qualified Drool.UI.ViewOptions as ViewOptions
import qualified Drool.UI.GLWindow as GLWindow

-- import qualified Drool.Utils.MFifoQ as MFifoQ
-- import Control.Monad.Queue.Allison

main :: IO()
main = do
  _ <- Gtk.initGUI

  -- emptyBuffer <- DT.newSignalBuffer
  emptySignal <- DT.newSignal
  let emptySignalList = (DT.newSignalList (50::Integer) emptySignal)
  signalBuffer <- newIORef (DT.CSignalList emptySignalList)
  contextSettings <- newIORef(
    DT.ContextSettings { DT.translation = undefined,
                         DT.rotation = (0.0::GLfloat, 0.0::GLfloat, 0.0::GLfloat),
                         DT.angle = (0.0::GLfloat),
                         DT.scaling = 0.5,
                         DT.gridOpacity = 0.8,
                         DT.surfaceOpacity = 0.3,
                         DT.renderPerspective = DT.Isometric,
                         DT.signalBuf = signalBuffer } )

  -- samples as list
  let sampleList = [ sin (x/50) | x <- [0..512] ] :: [GLfloat]


{-
  -- Construct signal buffer with 10 signals (10 times the same signal)
  signal <- (newListArray (0, length samples - 1) samples)::IO (IOArray Int GLfloat)
  signals <- ((newArray(0,9) ((DT.CSignal signal)::DT.Signal))::IO (IOArray Int DT.Signal))
  let buf = (DT.CSignalBuffer signals)::DT.SignalBuffer
-}
 settings <- readIORef contextSettings
--  contextSettings $=! settings { DT.signalBuf = buf }

  -- let repeatSignal sig = sig : (repeatSignal sig)
  -- let signals = (map (\s -> DT.CSignal s) (take 50 (repeatSignal signal)))
  -- contextSettings $=! settings { DT.signalBuf = DT.CSignalList signals }

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
  updateSamplesTimer <- Gtk.timeoutAddFull (do

      cSettings <- readIORef contextSettings

      -- Sample list to array:
      newSignal <- (newListArray (0, length sampleList - 1) sampleList)::IO (IOArray Int GLfloat)
      -- pop first signal in buffer, append sample list as new signal to buffer:
      modifyIORef signalBuffer (\list -> DT.CSignalList( (drop 1 (DT.signalList list)) ++ [ DT.CSignal newSignal ]) )

      contextSettings $=! cSettings { DT.angle = (DT.angle cSettings)+1 }
      return True)
    Gtk.priorityDefaultIdle 17

  -- Remove timer for redrawing canvas when closing window:
  Gtk.onDestroy mainWindow (Gtk.timeoutRemove updateSamplesTimer)

  -- Display window:
  Gtk.widgetShowAll mainWindow

  -- Enter GUI main loop:
  Gtk.mainGUI
