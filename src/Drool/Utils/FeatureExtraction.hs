-----------------------------------------------------------------------------
--
-- Module      :  Drool.Utils.FeatureExtraction
-- Copyright   :  Tobias Fuchs
-- License     :  AllRightsReserved
--
-- Maintainer  :  twh.fuchs@gmail.com
-- Stability   :  experimental
-- Portability :  POSIX
--
-- |
--
-----------------------------------------------------------------------------

{-# OPTIONS -O2 -Wall #-}


module Drool.Utils.FeatureExtraction (
    extractSignalFeatures, 
    SignalFeatures(..), 
    SignalFeaturesList(..), 
    FeatureExtractionSettings(..), 
    emptyFeatures, 
    FeatureTarget(..), 
    featureTargetIndex, 
    featureTargetFromIndex
) where 

import Drool.Utils.SigGen as SigGen ( SValue, SignalGenerator(..) )


data SignalFeatures = SignalFeatures { totalEnergy :: Float, 
                                       bassEnergy :: Float } 

data FeatureExtractionSettings = FeatureExtractionSettings { maxBeatBand :: Int } 

data FeatureTarget = NoTarget | LocalTarget | GlobalTarget | GlobalAndLocalTarget
  deriving ( Eq, Show )

newtype SignalFeaturesList = SignalFeaturesList { signalFeaturesList :: [SignalFeatures] } 

-- Extract features from given signal, depending on settings of extraction 
-- and signal generator. 
-- Very simplicistic, just a minimal implementation of the interface. 
extractSignalFeatures :: [SValue] -> FeatureExtractionSettings -> SigGen.SignalGenerator -> SignalFeatures
extractSignalFeatures samples settings siggen = SignalFeatures { totalEnergy = tEnergy, bassEnergy = bEnergy }
  where tEnergy = realToFrac $ (sum samples) / ( fromIntegral (SigGen.numSamples siggen) )
        bEnergy = realToFrac $ (sum $ take nBassSamples samples ) / ( fromIntegral nBassSamples )
        nBassSamples = maxBeatBand settings

emptyFeatures :: SignalFeatures
emptyFeatures = SignalFeatures { totalEnergy = 0, bassEnergy = 0 } 

featureTargetIndex :: FeatureTarget -> Int
featureTargetIndex target = case target of 
  NoTarget             -> 0
  LocalTarget          -> 1
  GlobalTarget         -> 2
  GlobalAndLocalTarget -> 3
  _ -> 0

featureTargetFromIndex :: Int -> FeatureTarget
featureTargetFromIndex idx = case idx of 
  0 -> NoTarget
  1 -> LocalTarget
  2 -> GlobalTarget
  3 -> GlobalAndLocalTarget
  _ -> NoTarget

