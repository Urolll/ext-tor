{-# LANGUAGE GADTs, ScopedTypeVariables, TypeApplications, AllowAmbiguousTypes #-}
module ChainComplex where

import RingModule

data ChainComplex m r where
  ChainComplex :: { modules :: [m]
                  , differentials :: [m -> m]
                  } -> ChainComplex m r

verifyComplex :: forall m r. (Module m r, Eq m) => ChainComplex m r -> Bool
verifyComplex (ChainComplex _ diffs) =
  all (\(d1, d2) -> all (\x -> d1 (d2 x) == zero @m @r) (elements @m @r))
      (zip diffs (tail diffs))

z2Resolution :: ChainComplex Z2Module Z2
z2Resolution = ChainComplex [Z2Module (Z2 0), Z2Module (Z2 0)]
                            [\_ -> zero @Z2Module @Z2]
