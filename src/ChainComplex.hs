{-# LANGUAGE GADTs, ScopedTypeVariables, TypeApplications #-}
module ChainComplex where

import RingModule

data ChainComplex m r where
  ChainComplex :: 
    { modules :: [m]
    , differentials :: [m -> m]
    } -> ChainComplex m r

verifyComplex :: forall m r. (Module m r, Eq m) => ChainComplex m r -> Bool
verifyComplex (ChainComplex mods diffs) =
  all (\(d1, d2) -> all (\x -> d1 (d2 x) == zero @m @r) (elements (head mods)))
  (zip diffs (tail diffs))
  where
    elements :: m -> [m]
    elements _ = 
      case zero @m @r of
        Z2Module _ -> [Z2Module (Z2 0), Z2Module (Z2 1)]
        FreeModule _ -> [FreeModule [(0, multId @r)]]
        _ -> error "Unsupported module type"

z2Resolution :: ChainComplex Z2Module Z2
z2Resolution = ChainComplex
  [ Z2Module (Z2 0)
  , Z2Module (Z2 0)
  ]
  [\_ -> zero @Z2Module @Z2]
