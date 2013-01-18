-----------------------------------------------------------------------------
--
-- Module      :  MenuBar
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

module Drool.UI.Menubar (
  initComponent
) where

import Data.IORef

import qualified Graphics.UI.Gtk as Gtk
import qualified Graphics.UI.Gtk.Builder as GtkBuilder

import qualified Drool.ApplicationContext as AC
import qualified Drool.ContextObjects as AC

import qualified Drool.UI.ViewOptions as ViewOptions
import qualified Drool.UI.TransformationOptions as TransformationOptions

initComponent :: GtkBuilder.Builder -> IORef AC.ContextSettings -> IORef AC.ContextObjects -> IO Bool
initComponent gtkBuilder contextSettings _ = do
  
  menuItem_FileClose <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToMenuItem "menuitemQuit"
  _ <- Gtk.on menuItem_FileClose Gtk.menuItemActivate $ do
    mainWindow <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToWindow "mainWindow"
    putStrLn "Exiting" 
    Gtk.widgetDestroy mainWindow
    
  menuItem_SaveSettingsAs <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToMenuItem "menuitemSaveSettingsAs"
  _ <- Gtk.on menuItem_SaveSettingsAs Gtk.menuItemActivate $ do
    cSettings <- readIORef contextSettings

    fileFilter <- Gtk.fileFilterNew
    Gtk.fileFilterAddPattern fileFilter "*.drool"
    Gtk.fileFilterSetName fileFilter "Drool Settings"

    mainWindow <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToWindow "mainWindow"
    fileChooseDlg <- Gtk.fileChooserDialogNew (Just "Save Settings") 
                                              (Just mainWindow) 
                                              Gtk.FileChooserActionSave 
                                              [("Save", Gtk.ResponseOk), ("Cancel", Gtk.ResponseCancel)]
    Gtk.fileChooserSetFilter fileChooseDlg fileFilter
    Gtk.widgetShowAll fileChooseDlg

    response <- Gtk.dialogRun fileChooseDlg
    case response of 
      Gtk.ResponseOk -> do filepath <- Gtk.fileChooserGetFilename fileChooseDlg
                           putStrLn $ "Saving settings to " ++ show filepath
                           case filepath of 
                             Just path -> do _ <- AC.saveContextSettingsAs cSettings path
                                             return ()
                             Nothing -> putStrLn $ "No path given"
      _ -> putStrLn "Cancelled saving settings"

    Gtk.widgetDestroy fileChooseDlg

  menuItem_LoadSettings <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToMenuItem "menuitemLoadSettings"
  _ <- Gtk.on menuItem_LoadSettings Gtk.menuItemActivate $ do
    
    fileFilter <- Gtk.fileFilterNew
    Gtk.fileFilterAddPattern fileFilter "*.drool"
    Gtk.fileFilterSetName fileFilter "Drool Settings"

    mainWindow <- GtkBuilder.builderGetObject gtkBuilder Gtk.castToWindow "mainWindow"
    fileChooseDlg <- Gtk.fileChooserDialogNew (Just "Save Settings") 
                                              (Just mainWindow) 
                                              Gtk.FileChooserActionOpen
                                              [("Open", Gtk.ResponseOk), ("Cancel", Gtk.ResponseCancel)]
    Gtk.fileChooserSetFilter fileChooseDlg fileFilter
    Gtk.widgetShowAll fileChooseDlg

    response <- Gtk.dialogRun fileChooseDlg
    case response of 
      Gtk.ResponseOk -> do filepath <- Gtk.fileChooserGetFilename fileChooseDlg
                           putStrLn $ "Loading settings from " ++ show filepath
                           case filepath of 
                             Just path -> do cSettings <- readIORef contextSettings
                                             let curAmp = AC.signalAmpDb cSettings
                                             settings <- AC.loadContextSettings path
                                             -- Preserve signal amplification setting: 
                                             let newSettings = settings { AC.signalAmpDb = curAmp }
                                             writeIORef contextSettings newSettings
                                             _ <- ViewOptions.updateSettings gtkBuilder settings
                                             _ <- TransformationOptions.updateSettings gtkBuilder settings
                                             return ()
                             Nothing -> putStrLn $ "No path given"
      _ -> putStrLn "Cancelled loading settings"

    Gtk.widgetDestroy fileChooseDlg

  return True
