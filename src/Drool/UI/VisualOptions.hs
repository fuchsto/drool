-----------------------------------------------------------------------------
--
-- Module      :  VisualOptions
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

module Drool.UI.VisualOptions (
  initComponent, updateSettings
) where

 
import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder
import Data.IORef
import qualified Drool.ApplicationContext as AC
import qualified Drool.ContextObjects as AC
import qualified Drool.UI.GtkHelpers as GH
import Drool.UI.Visuals as Visuals ( VisualModel(..), visualModelFromIndex )
import Drool.UI.Visuals.Blank as Visuals 
import Drool.UI.Visuals.Spheres as Visuals 
import Drool.UI.Visuals.Tunnel as Visuals 
import Drool.UI.Visuals.FFTSurface as Visuals 

-- Initializes GUI component for view options.
-- Expects a GtkBuilder instance and default context settings. 
initComponent :: GtkBuilder.Builder -> IORef AC.ContextSettings -> IORef AC.ContextObjects -> IO Bool
initComponent gtkBuilder contextSettings contextObjects = do
  putStrLn "Initializing VisualOptions component"

  comboboxVisualForeground <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToComboBox "comboboxVisualForegroundModel"
  _ <- Gtk.on comboboxVisualForeground Gtk.changed $ do 
    modelIdx <- Gtk.comboBoxGetActive comboboxVisualForeground
    cObjects <- readIORef contextObjects
    let visualIORef = AC.visualForeground cObjects
    spheresInitState         <- Visuals.newSpheresState contextSettings 
    spheresInitStateIORef    <- newIORef spheresInitState
    fftSurfaceInitState      <- Visuals.newFFTSurfaceState contextSettings 
    fftSurfaceInitStateIORef <- newIORef fftSurfaceInitState
    tunnelInitState          <- Visuals.newTunnelState contextSettings 
    tunnelInitStateIORef     <- newIORef tunnelInitState
    _ <- modifyIORef visualIORef ( \_ -> case Visuals.visualModelFromIndex modelIdx of
                                                Visuals.FFTSurfaceModel -> Visuals.newFFTSurfaceVisual contextSettings fftSurfaceInitStateIORef
                                                Visuals.TunnelModel     -> Visuals.newTunnelVisual contextSettings tunnelInitStateIORef
                                                Visuals.SpheresModel    -> Visuals.newSpheresVisual contextSettings spheresInitStateIORef
                                                _                       -> Visuals.newBlankVisual )
    return ()
  
  comboboxBlendingFMSource <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToComboBox "comboboxBlendingFMSource"
  _ <- Gtk.on comboboxBlendingFMSource Gtk.changed $ do
    modeIdx <- Gtk.comboBoxGetActive comboboxBlendingFMSource
    modifyIORef contextSettings ( \settings -> settings { AC.blendModeFMSourceIdx = modeIdx } )
  comboboxBlendingFMFrameBuffer <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToComboBox "comboboxBlendingFMFrameBuffer"
  _ <- Gtk.on comboboxBlendingFMFrameBuffer Gtk.changed $ do
    modeIdx <- Gtk.comboBoxGetActive comboboxBlendingFMFrameBuffer
    modifyIORef contextSettings ( \settings -> settings { AC.blendModeFMFrameBufferIdx = modeIdx } )

  comboboxVisualMiddleground <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToComboBox "comboboxVisualMiddlegroundModel"
  _ <- Gtk.on comboboxVisualMiddleground Gtk.changed $ do
    modelIdx <- Gtk.comboBoxGetActive comboboxVisualMiddleground
    cObjects <- readIORef contextObjects
    let visualIORef = AC.visualMiddleground cObjects
    spheresInitState         <- Visuals.newSpheresState contextSettings 
    spheresInitStateIORef    <- newIORef spheresInitState
    fftSurfaceInitState      <- Visuals.newFFTSurfaceState contextSettings 
    fftSurfaceInitStateIORef <- newIORef fftSurfaceInitState
    tunnelInitState          <- Visuals.newTunnelState contextSettings 
    tunnelInitStateIORef     <- newIORef tunnelInitState
    _ <- modifyIORef visualIORef ( \_ -> case Visuals.visualModelFromIndex modelIdx of
                                                Visuals.FFTSurfaceModel -> Visuals.newFFTSurfaceVisual contextSettings fftSurfaceInitStateIORef 
                                                Visuals.TunnelModel     -> Visuals.newTunnelVisual contextSettings tunnelInitStateIORef
                                                Visuals.SpheresModel    -> Visuals.newSpheresVisual contextSettings spheresInitStateIORef
                                                _                       -> Visuals.newBlankVisual )
    return ()

  comboboxBlendingMBSource <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToComboBox "comboboxBlendingMBSource"
  _ <- Gtk.on comboboxBlendingMBSource Gtk.changed $ do
    modeIdx <- Gtk.comboBoxGetActive comboboxBlendingMBSource
    modifyIORef contextSettings ( \settings -> settings { AC.blendModeMBSourceIdx = modeIdx } )
  comboboxBlendingMBFrameBuffer <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToComboBox "comboboxBlendingMBFrameBuffer"
  _ <- Gtk.on comboboxBlendingMBFrameBuffer Gtk.changed $ do
    modeIdx <- Gtk.comboBoxGetActive comboboxBlendingMBFrameBuffer
    modifyIORef contextSettings ( \settings -> settings { AC.blendModeMBFrameBufferIdx = modeIdx } )

  comboboxVisualBackground <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToComboBox "comboboxVisualBackgroundModel"
  _ <- Gtk.on comboboxVisualBackground Gtk.changed $ do
    modelIdx <- Gtk.comboBoxGetActive comboboxVisualBackground
    cObjects <- readIORef contextObjects
    let visualIORef = AC.visualBackground cObjects
    spheresInitState         <- Visuals.newSpheresState contextSettings 
    spheresInitStateIORef    <- newIORef spheresInitState
    fftSurfaceInitState      <- Visuals.newFFTSurfaceState contextSettings 
    fftSurfaceInitStateIORef <- newIORef fftSurfaceInitState
    tunnelInitState          <- Visuals.newTunnelState contextSettings 
    tunnelInitStateIORef     <- newIORef tunnelInitState
    _ <- modifyIORef visualIORef ( \_ -> case Visuals.visualModelFromIndex modelIdx of
                                                Visuals.FFTSurfaceModel -> Visuals.newFFTSurfaceVisual contextSettings fftSurfaceInitStateIORef
                                                Visuals.TunnelModel     -> Visuals.newTunnelVisual contextSettings tunnelInitStateIORef
                                                Visuals.SpheresModel    -> Visuals.newSpheresVisual contextSettings spheresInitStateIORef
                                                _                       -> Visuals.newBlankVisual )
    return ()
    
  return True

updateSettings :: GtkBuilder.Builder -> AC.ContextSettings -> IO Bool
updateSettings gtkBuilder settings = do 
  return True
  

