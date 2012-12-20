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
import qualified Drool.ContextObjects as CO
import qualified Drool.Utils.FeatureExtraction as FE
import qualified Drool.Utils.Conversions as Conv ( freqToMs )

-- Initializes GUI component for transformation options.
-- Expects a GtkBuilder instance and default context settings. 
initComponent :: GtkBuilder.Builder -> IORef AC.ContextSettings -> IORef CO.ContextObjects -> IO Bool
initComponent gtkBuilder contextSettings contextObjects = do
  putStrLn "Initializing FeatureExtractionOptions component"

  defaultSettings <- readIORef contextSettings

  adjBeatMaxBandSamples <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToAdjustment "adjBeatMaxBandSamples"
  Gtk.adjustmentSetValue adjBeatMaxBandSamples (fromIntegral $ AC.maxBeatBand defaultSettings)
  _ <- Gtk.onValueChanged adjBeatMaxBandSamples $ do 
    val <- Gtk.adjustmentGetValue adjBeatMaxBandSamples
    settings <- readIORef contextSettings
    contextSettings $=! settings { AC.maxBeatBand = round val }

  let updateBeatEnergyCallback = ( do
      cSettings <- readIORef contextSettings
      cObjects  <- readIORef contextObjects
      let signalFeaturesBuffer = CO.featuresBuf cObjects
      featuresBuf <- readIORef signalFeaturesBuffer
      let curFeatures = head $ FE.signalFeaturesList featuresBuf 
          beatEnergy  = FE.bassEnergy curFeatures
      
      fieldBeatEnergy <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToLabel "labelBeatEnergyValue"
      Gtk.labelSetText fieldBeatEnergy $ show beatEnergy

      return True )
  _ <- Gtk.timeoutAddFull updateBeatEnergyCallback Gtk.priorityDefaultIdle (Conv.freqToMs 1)
  
  return True


