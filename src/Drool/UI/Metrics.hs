-----------------------------------------------------------------------------
--
-- Module      :  Drool.UI.Metrics
-- Copyright   :
-- License     :  GPL v3
--
-- Maintainer  :  Tobias Fuchs
-- Stability   :  experimental
-- Portability :  Win32, POSIX
--
-- |
--
-----------------------------------------------------------------------------

{-# OPTIONS -O2 -Wall #-}

module Drool.UI.Metrics (
  initComponent
) where

import Data.IORef

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder
import qualified Drool.Utils.Conversions as Conv ( freqToMs )

import qualified Drool.ApplicationContext as AC

-- Initializes GUI component for transformation options.
-- Expects a GtkBuilder instance and default context settings. 
initComponent :: GtkBuilder.Builder -> IORef AC.ContextSettings -> IORef AC.ContextObjects -> IO Bool
initComponent gtkBuilder contextSettings _ = do
  putStrLn "Initializing Metrics component"

  defaultSettings <- readIORef contextSettings

  let updateCallback = ( do
      cSettings <- readIORef contextSettings
      let metrics = AC.metrics cSettings
      
      fieldLatency <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToLabel "fieldLatency"
      Gtk.labelSetText fieldLatency $ show (AC.latency metrics) 

      return True )
  updateTimer <- Gtk.timeoutAddFull updateCallback Gtk.priorityDefaultIdle (Conv.freqToMs 1)

  return True
