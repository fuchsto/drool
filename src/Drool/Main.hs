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
import qualified Drool.Utils.Conversions as Conv
import qualified Drool.ApplicationContext as AC

import qualified Drool.UI.ViewOptions as ViewOptions
import qualified Drool.UI.SamplingOptions as SamplingOptions
import qualified Drool.UI.SignalSource as SignalSource
import qualified Drool.UI.GLWindow as GLWindow



main :: IO()
main = do
  _ <- Gtk.initGUI

  let signalBufferSize = 50
  emptySignal <- DT.newSignal
  -- Initialize signal buffer with signalBufferSize empty signals:
  let emptySignalBuffer = (DT.newSignalList (signalBufferSize::Int) emptySignal)
  signalBuffer <- newIORef (DT.CSignalList emptySignalBuffer)
  -- Initialize test signal generator:
  let transform pLength t sample = (SigGen.sine pLength t) * sample
  let defaultSiggen = SigGen.CSignalGenerator { SigGen.baseSignal = SigGen.CBaseSignal SigGen.dirac,
                                                SigGen.ampTransformation = SigGen.CAmpTransformation transform,
                                                SigGen.signalPeriodLength = 3,
                                                SigGen.envelopePeriodLength = 40,
                                                SigGen.numSamples = 50 }

  contextSettings <- newIORef(
    AC.ContextSettings { AC.samplingFrequency = 20,
                         AC.renderingFrequency = 50,
                         AC.translation = undefined,
                         AC.rotation = (0.0::GLfloat, 0.0::GLfloat, 0.0::GLfloat),
                         AC.angle = (0.0::GLfloat),
                         AC.scaling = 30,
                         AC.gridOpacity = 15,
                         AC.surfaceOpacity = 13,
                         AC.surfaceColor = Color3 (62.0/255) (187.0/255) (1::GLfloat),
                         AC.lightColor = Color3 (239.0/255) (19.0/255) (19.0/255.0::GLfloat) ,
                         AC.gridColor = Color3 (142.0/255) 1 (58.0/255::GLfloat),
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

  _ <- SamplingOptions.initComponent builder contextSettings
  _ <- ViewOptions.initComponent builder contextSettings
  _ <- SignalSource.initComponent builder contextSettings

  let updateCallback count = (do

      cSettings <- readIORef contextSettings

      let siggen = AC.signalGenerator cSettings

      -- let genSampleList = take (SigGen.numSamples siggen) (map (\s -> (realToFrac s) :: GLfloat) (SigGen.genSignal siggen count))
      let genSampleList = take (SigGen.numSamples siggen) (SigGen.genSignal siggen count)

      -- Sample list to array:
      newSignal <- (newListArray (0, length genSampleList - 1) genSampleList)::IO (IOArray Int GLfloat)
      -- pop first signal in buffer, append sample list as new signal to buffer:
      modifyIORef signalBuffer (\list -> DT.CSignalList( (drop 1 (DT.signalList list)) ++ [ DT.CSignal newSignal ]) )

      contextSettings $=! cSettings { AC.angle = (AC.angle cSettings)+1 }

      -- Start a new timeout with incremented count:
      let timeoutMs = (Conv.freqToMs $ AC.samplingFrequency cSettings)
      _ <- Gtk.timeoutAddFull (updateCallback (count+1)) Gtk.priorityDefaultIdle timeoutMs

      -- do not run this callback again:
      return False)

  -- Initialize sample timer with t=0 and start immediately:
  updateSamplesTimer <- Gtk.timeoutAddFull (updateCallback 0) Gtk.priorityDefaultIdle 1

  -- Remove sample timer when closing application:
  _ <- Gtk.onDestroy mainWindow (Gtk.timeoutRemove updateSamplesTimer)

  -- Display window:
  _ <- Gtk.widgetShowAll mainWindow

  -- Enter GUI main loop:
  Gtk.mainGUI
