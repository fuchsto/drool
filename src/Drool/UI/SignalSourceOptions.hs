-----------------------------------------------------------------------------
--
-- Module      :  ViewOptions
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

{-# OPTIONS -O2 -Wall #-}

module Drool.UI.SignalSourceOptions (
  initComponent
) where

import Data.IORef

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder

import Graphics.Rendering.OpenGL

import qualified Drool.ApplicationContext as AC ( ContextSettings(..) )
import qualified Drool.ContextObjects as AC
import qualified Drool.Utils.SigGen as SigGen
import qualified Drool.Types as DT


-- Initializes GUI component for signal source.
-- Expects a GtkBuilder instance.
initComponent :: GtkBuilder.Builder -> IORef AC.ContextSettings -> IORef AC.ContextObjects -> IO Bool
initComponent gtkBuilder contextSettings contextObjects = do
  putStrLn "Initializing SignalSourceOptions component"

  -- defaultSettings <- readIORef contextSettings
  defaultObjects  <- readIORef contextObjects

  -- Initialize tab "Test signal":

  cbox_testsignal_generator <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToComboBox "comboboxGenerator"
  cbox_testsignal_envelope <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToComboBox "comboboxEnvelope"

  spinbutton_signalPeriods <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToSpinButton "spinbuttonSignalPeriods"
  spinbutton_envelopePeriods <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToSpinButton "spinbuttonEnvelopePeriods"

  -- Apply default settings to UI components:
  let defaultSigGen = AC.signalGenerator defaultObjects
  Gtk.spinButtonSetValue spinbutton_signalPeriods (fromIntegral (SigGen.signalPeriodLength defaultSigGen))
  Gtk.spinButtonSetValue spinbutton_envelopePeriods (fromIntegral (SigGen.envelopePeriodLength defaultSigGen))
  Gtk.comboBoxSetActive cbox_testsignal_generator 3
  Gtk.comboBoxSetActive cbox_testsignal_envelope 0

  button_activateSignalGenerator <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonActivateSignalGenerator"
  _ <- Gtk.onClicked button_activateSignalGenerator $ do
    cSettings <- readIORef contextSettings
    cObjects  <- readIORef contextObjects

    sigGenIdx <- Gtk.comboBoxGetActive cbox_testsignal_generator
    sigEnvIdx <- Gtk.comboBoxGetActive cbox_testsignal_envelope

    sigPeriods <- Gtk.spinButtonGetValueAsInt spinbutton_signalPeriods
    envPeriods <- Gtk.spinButtonGetValueAsInt spinbutton_envelopePeriods

    putStrLn $ "Signal generator: " ++ (show sigGenIdx) ++ " envelope: " ++ (show sigEnvIdx)
    putStrLn $ "Signal periods: " ++ (show sigPeriods) ++ " envelope periods: " ++ (show envPeriods)

    let currentSigGen = AC.signalGenerator cObjects
    -- Update signal generator from settings:
    let siggen = currentSigGen { SigGen.baseSignal = SigGen.CBaseSignal (SigGen.signalFunFromIndex sigGenIdx),
                                 SigGen.ampTransformation = SigGen.CAmpTransformation (SigGen.envelopeFunFromIndex sigEnvIdx),
                                 SigGen.signalPeriodLength = sigPeriods,
                                 SigGen.envelopePeriodLength = envPeriods }
    -- Activate signal generator in application context:
    contextSettings $=! cSettings { AC.signalSource = DT.TestSignal }
    contextObjects  $=! cObjects { AC.signalGenerator = siggen } 

  button_activateMicRecording <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToButton "buttonStartMicRecording"
  _ <- Gtk.onClicked button_activateMicRecording $ do
    cSettings <- readIORef contextSettings
    contextSettings $=! cSettings { AC.signalSource = DT.Microphone }

  return True

