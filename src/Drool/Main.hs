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

import qualified Drool.Utils.SigGen as SigGen


main :: IO()
main = do
  _ <- Gtk.initGUI

  let signalBufferSize = 50
  emptySignal <- DT.newSignal
  -- Initialize signal buffer with signalBufferSize empty signals:
  let emptySignalBuffer = (DT.newSignalList (signalBufferSize::Integer) emptySignal)
  signalBuffer <- newIORef (DT.CSignalList emptySignalBuffer)
  contextSettings <- newIORef(
    DT.ContextSettings { DT.translation = undefined,
                         DT.rotation = (0.0::GLfloat, 0.0::GLfloat, 0.0::GLfloat),
                         DT.angle = (0.0::GLfloat),
                         DT.scaling = 0.5,
                         DT.gridOpacity = 0.8,
                         DT.surfaceOpacity = 0.3,
                         DT.renderPerspective = DT.Isometric,
                         DT.signalBuf = signalBuffer } )

  -- Load UI configuration from GtkBuilder file:
  builder <- GtkBuilder.builderNew
  GtkBuilder.builderAddFromFile builder "droolui-test.glade"

  -- Instatiate window from GtkBuilder file:
  mainWindow <- GtkBuilder.builderGetObject builder Gtk.castToWindow "mainWindow"


  openVisualizerButton <- GtkBuilder.builderGetObject builder Gtk.castToButton "buttonVisualizer"
  _ <- Gtk.onClicked openVisualizerButton $ do
    GLWindow.initComponent builder contextSettings

  -- Application exit callback (quits main loop):
  _ <- Gtk.onDestroy mainWindow Gtk.mainQuit

  -- Define button callbacks:
  menuItem_FileClose <- GtkBuilder.builderGetObject builder Gtk.castToMenuItem "menuitemFileQuit"
  _ <- Gtk.on menuItem_FileClose Gtk.menuItemActivate $ do
    putStrLn "Exiting"
    Gtk.widgetDestroy mainWindow

  _ <- ViewOptions.initComponent builder contextSettings

  -- Playing around with signal generators:

  let transform pLength t sample = (SigGen.sine pLength t) * sample
  -- let transform pLength t sample = sample + fromIntegral(t)/100.0
  let siggen = SigGen.CSignalGenerator { SigGen.baseSignal = SigGen.CBaseSignal SigGen.sine,
                                         SigGen.ampTransformation = SigGen.CAmpTransformation transform,
                                         SigGen.signalPeriodLength = 100,
                                         SigGen.transPeriodLength = 150,
                                         SigGen.numSamples = 200 }

  let updateCallback count = (do

      cSettings <- readIORef contextSettings

      let genSampleList = take 200 (map (\s -> (realToFrac s) :: GLfloat) (SigGen.genSignal siggen count))

      -- Sample list to array:
      newSignal <- (newListArray (0, length genSampleList - 1) genSampleList)::IO (IOArray Int GLfloat)
      -- pop first signal in buffer, append sample list as new signal to buffer:
      modifyIORef signalBuffer (\list -> DT.CSignalList( (drop 1 (DT.signalList list)) ++ [ DT.CSignal newSignal ]) )

      contextSettings $=! cSettings { DT.angle = (DT.angle cSettings)+1 }

      -- Start a new timeout with incremented count:
      _ <- Gtk.timeoutAddFull (updateCallback (count+1)) Gtk.priorityDefaultIdle 20

      -- do not run this callback again:
      return False)

  -- Redraw canvas every 3ms:
  updateSamplesTimer <- Gtk.timeoutAddFull (updateCallback 0) Gtk.priorityDefaultIdle 20

  -- Remove timer for redrawing canvas when closing window:
  _ <- Gtk.onDestroy mainWindow (Gtk.timeoutRemove updateSamplesTimer)

  -- Display window:
  _ <- Gtk.widgetShowAll mainWindow

  -- Enter GUI main loop:
  Gtk.mainGUI
