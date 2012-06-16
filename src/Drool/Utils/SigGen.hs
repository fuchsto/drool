-----------------------------------------------------------------------------
--
-- Module      :  Drool.Utils.SigGen
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

module Drool.Utils.SigGen (

) where

type SignalState = Int

nextSigState :: SignalState -> SignalState
nextSigState s = if s < 100 then s+1 else 0

type SigGenMonad = State [GFloat]
