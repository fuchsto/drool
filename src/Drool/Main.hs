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
import qualified Drool.Utils.SigGen as SigGen
import qualified Drool.ApplicationContext as AC

import qualified Drool.UI.ViewOptions as ViewOptions
import qualified Drool.UI.SignalSource as SignalSource
import qualified Drool.UI.GLWindow as GLWindow



main :: IO()
main = do
  _ <- Gtk.initGUI

  let signalBufferSize = 50
  emptySignal <- DT.newSignal
  -- Initialize signal buffer with signalBufferSize empty signals:
  let emptySignalBuffer = (DT.newSignalList (signalBufferSize::Integer) emptySignal)
  signalBuffer <- newIORef (DT.CSignalList emptySignalBuffer)
  -- Initialize test signal generator:
  let transform pLength t sample = (SigGen.sine pLength t) * sample
  let defaultSiggen = SigGen.CSignalGenerator { SigGen.baseSignal = SigGen.CBaseSignal SigGen.sine,
                                                SigGen.ampTransformation = SigGen.CAmpTransformation transform,
                                                SigGen.signalPeriodLength = 80,
                                                SigGen.transPeriodLength = 30,
                                                SigGen.numSamples = 200 }

  contextSettings <- newIORef(
    AC.ContextSettings { AC.translation = undefined,
                         AC.rotation = (0.0::GLfloat, 0.0::GLfloat, 0.0::GLfloat),
                         AC.angle = (0.0::GLfloat),
                         AC.scaling = 0.5,
                         AC.gridOpacity = 0.8,
                         AC.surfaceOpacity = 0.3,
                         AC.surfaceColor = Color3 0.7 0.2 (0.7::GLfloat),
                         AC.gridColor = Color3 1 1 (1::GLfloat),
                         AC.renderPerspective = DT.Isometric,
                         AC.signalBuf = signalBuffer,
                         AC.signalGenerator = defaultSiggen } )

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

  _ <- SignalSource.initComponent builder contextSettings


  let updateCallback count = (do

      cSettings <- readIORef contextSettings

      let siggen = AC.signalGenerator cSettings

      let genSampleList = take 200 (map (\s -> (realToFrac s) :: GLfloat) (SigGen.genSignal siggen count))

      -- Sample list to array:
      newSignal <- (newListArray (0, length genSampleList - 1) genSampleList)::IO (IOArray Int GLfloat)
      -- pop first signal in buffer, append sample list as new signal to buffer:
      modifyIORef signalBuffer (\list -> DT.CSignalList( (drop 1 (DT.signalList list)) ++ [ DT.CSignal newSignal ]) )

      contextSettings $=! cSettings { AC.angle = (AC.angle cSettings)+1 }

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
