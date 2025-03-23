{-# LANGUAGE GADTs #-}
module ChainComplex where

import RingModule

data ChainComplex m r where
  ChainComplex ::
    { moduleN   :: m
    , moduleNm1 :: m
    , diff      :: m -> m
    } -> ChainComplex m r

z2Resolution :: ChainComplex (FreeModule Z2) Z2
z2Resolution = ChainComplex
  { moduleN = FreeModule (Z2 1) (Z2 1)
  , moduleNm1 = FreeModule (Z2 1) (Z2 1)
  , diff = \(FreeModule a b) -> FreeModule (add a a) (add b b)
  }

verifyComplex :: (Module m r, Eq m) => ChainComplex m r -> Bool
verifyComplex cc = all isZero [diff cc (diff cc x) | x <- basis]
  where
    basis = [zero, smul multId zero]
    isZero x = x == zero
