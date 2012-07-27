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
  initComponent, 
  updateSettings
) where

import Data.IORef

import qualified Graphics.UI.Gtk.Builder as GtkBuilder
import qualified Drool.ApplicationContext as AC
import qualified Drool.UI.GtkHelpers as GH


-- Initializes GUI component for transformation options.
-- Expects a GtkBuilder instance and default context settings. 
initComponent :: GtkBuilder.Builder -> IORef AC.ContextSettings -> IORef AC.ContextObjects -> IO Bool
initComponent gtkBuilder contextSettings _ = do
  putStrLn "Initializing TransformationOptions component"

  defaultSettings <- readIORef contextSettings

  GH.bindAdjustment "adjNumFFTBands" gtkBuilder contextSettings (\v settings -> settings { AC.numFFTBands = round v }) 
  GH.bindAdjustment "adjIIRCoef"     gtkBuilder contextSettings (\v settings -> settings { AC.iirCoef = realToFrac v }) 
  GH.bindAdjustment "adjAmpDb"       gtkBuilder contextSettings (\v settings -> settings { AC.signalAmpDb = realToFrac v })

  GH.bindCheckButton "checkbuttonUseFFT" gtkBuilder contextSettings (\v settings -> settings { AC.fftEnabled = v })
  GH.bindCheckButton "checkbuttonUseIIR" gtkBuilder contextSettings (\v settings -> settings { AC.iirEnabled = v })
  GH.bindCheckButton "checkbuttonUseAmp" gtkBuilder contextSettings (\v settings -> settings { AC.ampEnabled = v })

  _ <- updateSettings gtkBuilder defaultSettings

  return True

updateSettings :: GtkBuilder.Builder -> AC.ContextSettings -> IO Bool
updateSettings gtkBuilder settings = do 
  GH.initAdjustment "adjNumFFTBands" gtkBuilder (realToFrac $ AC.numFFTBands settings)
  GH.initAdjustment "adjIIRCoef"     gtkBuilder (realToFrac $ AC.iirCoef settings)
  GH.initAdjustment "adjAmpDb"       gtkBuilder (realToFrac $ AC.signalAmpDb settings)

  GH.initCheckButton "checkbuttonUseFFT" gtkBuilder (AC.fftEnabled settings)
  GH.initCheckButton "checkbuttonUseIIR" gtkBuilder (AC.fftEnabled settings)
  GH.initCheckButton "checkbuttonUseAmp" gtkBuilder (AC.fftEnabled settings)
  
  return True

