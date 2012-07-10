-----------------------------------------------------------------------------
--
-- Module      :  FeatureExtractionOptions
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :  Tobias Fuchs
-- Stability   :  experimental
-- Portability :  Win32, POSIX
--
-- |
--
-----------------------------------------------------------------------------

{-# OPTIONS -O2 -Wall #-}

module Drool.UI.FeatureExtractionOptions (
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
  putStrLn "Initializing FeatureExtractionOptions component"

  defaultSettings <- readIORef contextSettings

  adjBeatMaxBandSamples <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBeatMaxBandSamples"
  Gtk.adjustmentSetValue adjBeatMaxBandSamples (fromIntegral $ AC.maxBeatBandSamples defaultSettings)
  _ <- Gtk.onValueChanged adjBeatMaxBandSamples $ do 
    val <- Gtk.adjustmentGetValue adjBeatMaxBandSamples
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.maxBeatBandSamples = round val }

  return True


