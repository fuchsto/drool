-----------------------------------------------------------------------------
--
-- Module      :  TransformationOptions
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

module Drool.UI.TransformationOptions (
  initComponent
) where

import Data.IORef

import Graphics.Rendering.OpenGL ( ($=!) )
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder

import qualified Drool.ApplicationContext as AC

-- Initializes GUI component for transformation options.
-- Expects a GtkBuilder instance and default context settings. 
initComponent :: GtkBuilder.Builder -> IORef AC.ContextSettings -> IORef AC.ContextObjects -> IO Bool
initComponent gtkBuilder contextSettings _ = do
  putStrLn "Initializing TransformationOptions component"

  defaultSettings <- readIORef contextSettings

  adjAmpDb <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjAmpDb"
  Gtk.adjustmentSetValue adjAmpDb (realToFrac $ AC.signalAmpDb defaultSettings)
  _ <- Gtk.onValueChanged adjAmpDb $ do
    val <- Gtk.adjustmentGetValue adjAmpDb
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.signalAmpDb = realToFrac val }

  adjNumFFTBands <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjNumFFTBands"
  Gtk.adjustmentSetValue adjNumFFTBands (realToFrac $ AC.numFFTBands defaultSettings)
  _ <- Gtk.onValueChanged adjNumFFTBands $ do
    val <- Gtk.adjustmentGetValue adjNumFFTBands
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.numFFTBands = round val }

  checkbuttonUseFFT <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToCheckButton "checkbuttonUseFFT"
  Gtk.toggleButtonSetActive checkbuttonUseFFT (AC.fftEnabled defaultSettings)
  _ <- Gtk.on checkbuttonUseFFT Gtk.toggled $ do 
    val <- Gtk.toggleButtonGetActive checkbuttonUseFFT
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.fftEnabled = val }

  checkbuttonUseAmp <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToCheckButton "checkbuttonUseAmp"
  Gtk.toggleButtonSetActive checkbuttonUseAmp (AC.fftEnabled defaultSettings)
  _ <- Gtk.on checkbuttonUseAmp Gtk.toggled $ do 
    val <- Gtk.toggleButtonGetActive checkbuttonUseAmp
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.ampEnabled = val }

  return True
