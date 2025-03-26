{-# LANGUAGE GADTs #-}
module ChainComplex where

import RingModule

data ChainComplex m r where
  ChainComplex ::
    { module0   :: m
    , module1 :: m
    , diff      :: m -> m
    } -> ChainComplex m r

z2Resolution :: ChainComplex (FreeModule Z2) Z2
z2Resolution = ChainComplex
  { module0 = FreeModule multId addId
  , module1 = zero
  , diff = \_ -> zero
  }

verifyComplex :: (Module m r, Eq m) => ChainComplex m r -> Bool
verifyComplex cc = all isZero [diff cc (diff cc x) | x <- basis]
  where
    basis = [zero, smul multId zero]
    isZero x = x == zero
