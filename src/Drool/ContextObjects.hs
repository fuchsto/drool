-----------------------------------------------------------------------------
--
-- Module      :  Drool.ContextObjects
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Drool.ContextObjects (
    ContextObjects(..)
) where

import Data.IORef ( IORef ) 
import qualified Control.Concurrent.MVar as MV ( MVar )
import qualified Control.Concurrent.Chan as CC ( Chan )
import qualified Control.Concurrent as CC ( ThreadId ) 
import Drool.Types as DT ( SignalList ) 
import Drool.Utils.SigGen as SigGen ( SignalGenerator ) 
import qualified Drool.Utils.FeatureExtraction as FE ( SignalFeaturesList ) 
import Drool.UI.Visuals

-- Non-serializable context settings. 
data ContextObjects = ContextObjects { visualForeground :: IORef Visual, 
                                       visualMiddleground :: IORef Visual, 
                                       visualBackground :: IORef Visual, 
                                       
                                       visualDefinitionChanged :: Bool, 
                                       samplingThreadId :: CC.ThreadId, 
                                       samplingSem :: MV.MVar Int, 
                                       numNewSignalsChan :: CC.Chan Int, 
                                       renderingSem :: MV.MVar Int, 
                                       signalBuf :: (IORef SignalList), 
                                       featuresBuf :: (IORef FE.SignalFeaturesList), 
                                       signalGenerator :: (SigGen.SignalGenerator) } 

