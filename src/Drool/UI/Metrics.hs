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
import qualified Drool.ContextObjects as CO

-- Initializes GUI component for transformation options.
-- Expects a GtkBuilder instance and default context settings. 
initComponent :: GtkBuilder.Builder -> IORef AC.ContextSettings -> IORef CO.ContextObjects -> IO Bool
initComponent gtkBuilder contextSettings _ = do
  putStrLn "Initializing Metrics component"

  let updateCallback = ( do
      cSettings <- readIORef contextSettings
      let metrics = AC.metrics cSettings
      
      fieldLatency <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToLabel "fieldLatency"
      Gtk.labelSetText fieldLatency $ show (AC.latency metrics) 

      return True )
  _ <- Gtk.timeoutAddFull updateCallback Gtk.priorityDefaultIdle (Conv.freqToMs 1)

  return True
