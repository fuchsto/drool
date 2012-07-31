-----------------------------------------------------------------------------
--
-- Module      :  FFTSurfaceDialog
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

module Drool.UI.Dialogs.FFTSurfaceDialog (
  initComponent, updateSettings
) where

import Data.IORef
import Control.Monad.Trans ( liftIO )

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder

import qualified Drool.UI.GtkHelpers as GH
import qualified Drool.ApplicationContext as AC
import qualified Drool.ContextObjects as CO

initComponent :: GtkBuilder.Builder -> IORef AC.ContextSettings -> IORef CO.ContextObjects -> IO Bool
initComponent gtkBuilder contextSettings _ = do
  putStrLn "Initializing FFTSurface dialog component"

  defaultSettings <- readIORef contextSettings

  fftSurfaceDlg <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToWindow "windowFFTSurface"
  Gtk.widgetShowAll fftSurfaceDlg

  GH.bindAdjustment "adjGridOpacity"    gtkBuilder contextSettings (\v settings -> settings { AC.gridOpacity = realToFrac v })
  GH.bindAdjustment "adjSurfaceOpacity" gtkBuilder contextSettings (\v settings -> settings { AC.surfaceOpacity = realToFrac v })

  -- Materials
  GH.bindColorAlphaButton "colorbuttonGridAmbient"     gtkBuilder contextSettings (\c s -> s { AC.gridMaterial = (AC.gridMaterial s) { AC.materialAmbient = c } } ) 
  GH.bindColorAlphaButton "colorbuttonGridDiffuse"     gtkBuilder contextSettings (\c s -> s { AC.gridMaterial = (AC.gridMaterial s) { AC.materialDiffuse = c } } ) 
  GH.bindColorAlphaButton "colorbuttonGridSpecular"    gtkBuilder contextSettings (\c s -> s { AC.gridMaterial = (AC.gridMaterial s) { AC.materialSpecular = c } } ) 
  GH.bindColorAlphaButton "colorbuttonGridEmission"    gtkBuilder contextSettings (\c s -> s { AC.gridMaterial = (AC.gridMaterial s) { AC.materialEmission = c } } ) 
  GH.bindAdjustment "adjGridShininess" gtkBuilder contextSettings (\v s -> s { AC.gridMaterial = (AC.gridMaterial s) { AC.materialShininess = realToFrac v } } )
  
  GH.bindColorAlphaButton "colorbuttonSurfaceAmbient"  gtkBuilder contextSettings (\c s -> s { AC.surfaceMaterial = (AC.surfaceMaterial s) { AC.materialAmbient = c } } ) 
  GH.bindColorAlphaButton "colorbuttonSurfaceDiffuse"  gtkBuilder contextSettings (\c s -> s { AC.surfaceMaterial = (AC.surfaceMaterial s) { AC.materialDiffuse = c } } ) 
  GH.bindColorAlphaButton "colorbuttonSurfaceSpecular" gtkBuilder contextSettings (\c s -> s { AC.surfaceMaterial = (AC.surfaceMaterial s) { AC.materialSpecular = c } } ) 
  GH.bindColorAlphaButton "colorbuttonSurfaceEmission" gtkBuilder contextSettings (\c s -> s { AC.surfaceMaterial = (AC.surfaceMaterial s) { AC.materialEmission = c } } ) 
  GH.bindAdjustment "adjSurfaceShininess" gtkBuilder contextSettings (\v s -> s { AC.surfaceMaterial = (AC.surfaceMaterial s) { AC.materialShininess = realToFrac v } } )
  
  let onLoad  _ = do _ <- Gtk.widgetHideAll fftSurfaceDlg
                     return () 
  
  let onSave  _ = do _ <- Gtk.widgetHideAll fftSurfaceDlg
                     return () 
  
  let onClose _ = do _ <- Gtk.widgetHideAll fftSurfaceDlg
                     return () 
  
  _ <- Gtk.on fftSurfaceDlg Gtk.deleteEvent $ do 
      liftIO $ Gtk.widgetHideAll fftSurfaceDlg
      return True -- Prevent destroy event from being emitted
  
  actionSave <- Gtk.builderGetObject gtkBuilder Gtk.castToAction "actionFFTSurfaceSettingsSave"
  _ <- Gtk.on actionSave Gtk.actionActivated $ do
         putStrLn "Saving"
  
  actionLoad <- Gtk.builderGetObject gtkBuilder Gtk.castToAction "actionFFTSurfaceSettingsLoad"
  _ <- Gtk.on actionLoad Gtk.actionActivated $ do
         putStrLn "Loading"
  
  GH.bindButton "buttonFFTSurfaceDlgClose" gtkBuilder contextSettings onClose
  
  _ <- updateSettings gtkBuilder defaultSettings
  
  return True


updateSettings :: GtkBuilder.Builder -> AC.ContextSettings -> IO Bool
updateSettings gtkBuilder settings = do 
  GH.initAdjustment "adjGridOpacity"    gtkBuilder (realToFrac $ AC.gridOpacity settings)
  GH.initAdjustment "adjSurfaceOpacity" gtkBuilder (realToFrac $ AC.surfaceOpacity settings)
  
  GH.initColorAlphaButton "colorbuttonGridAmbient"     gtkBuilder (AC.materialAmbient $ AC.gridMaterial settings)
  GH.initColorAlphaButton "colorbuttonGridDiffuse"     gtkBuilder (AC.materialDiffuse $ AC.gridMaterial settings)
  GH.initColorAlphaButton "colorbuttonGridSpecular"    gtkBuilder (AC.materialSpecular $ AC.gridMaterial settings)
  GH.initColorAlphaButton "colorbuttonGridEmission"    gtkBuilder (AC.materialEmission $ AC.gridMaterial settings)
  GH.initAdjustment "adjGridShininess" gtkBuilder ( realToFrac $ AC.materialShininess $ AC.gridMaterial settings )
  GH.initColorAlphaButton "colorbuttonSurfaceAmbient"  gtkBuilder (AC.materialAmbient $ AC.surfaceMaterial settings)
  GH.initColorAlphaButton "colorbuttonSurfaceDiffuse"  gtkBuilder (AC.materialDiffuse $ AC.surfaceMaterial settings)
  GH.initColorAlphaButton "colorbuttonSurfaceSpecular" gtkBuilder (AC.materialSpecular $ AC.surfaceMaterial settings)
  GH.initColorAlphaButton "colorbuttonSurfaceEmission" gtkBuilder (AC.materialEmission $ AC.surfaceMaterial settings)
  GH.initAdjustment "adjSurfaceShininess" gtkBuilder ( realToFrac $ AC.materialShininess $ AC.surfaceMaterial settings )

  return True
