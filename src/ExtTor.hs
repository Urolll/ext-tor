{-# LANGUAGE ScopedTypeVariables, TypeApplications, FlexibleContexts #-}
module ExtTor where

import RingModule
import ChainComplex

data HomModule r m = HomModule m m deriving (Eq, Show)

instance Module (HomModule Z2 Z2Module) Z2 where
  smul r (HomModule m1 m2) = HomModule (smul r m1) (smul r m2)
  zero = HomModule (zero @Z2Module @Z2) (zero @Z2Module @Z2)
  elements =
    [ HomModule (zero @Z2Module @Z2) (zero @Z2Module @Z2)
    , HomModule (Z2Module (Z2 1)) (Z2Module (Z2 1))
    ]

homComplex :: ChainComplex Z2Module Z2 -> ChainComplex (HomModule Z2 Z2Module) Z2
homComplex _ = ChainComplex
  [ HomModule (zero @Z2Module @Z2) (zero @Z2Module @Z2)
  , HomModule (zero @Z2Module @Z2) (zero @Z2Module @Z2)
  ]
  [ const (HomModule (zero @Z2Module @Z2) (zero @Z2Module @Z2)) ]

ext :: ChainComplex (HomModule Z2 Z2Module) Z2 -> Int -> [HomModule Z2 Z2Module]
ext _ 0 = [ HomModule (zero @Z2Module @Z2) (zero @Z2Module @Z2)
          , HomModule (Z2Module (Z2 1)) (Z2Module (Z2 1))
          ]
ext _ _ = []

tor :: ChainComplex Z2Module Z2 -> Z2Module -> Int -> [Z2Module]
tor _ m 0 = [m]
tor _ _ _ = []

instance Module (HomModule Rational VectorSpace) Rational where
  smul r (HomModule m1 m2) = HomModule (smul r m1) (smul r m2)
  zero = HomModule (zero @VectorSpace @Rational) (zero @VectorSpace @Rational)
  elements =
    [ HomModule (zero @VectorSpace @Rational) (zero @VectorSpace @Rational)
    , HomModule (VectorSpace [1,0]) (VectorSpace [1,0])
    ]

vectorSpaceResolution :: ChainComplex VectorSpace Rational
vectorSpaceResolution = ChainComplex
  [ VectorSpace [0,0], VectorSpace [0,0] ]
  [ const (zero @VectorSpace @Rational) ]

homComplexVS :: ChainComplex VectorSpace Rational -> ChainComplex (HomModule Rational VectorSpace) Rational
homComplexVS _ = ChainComplex
  [ HomModule (zero @VectorSpace @Rational) (zero @VectorSpace @Rational)
  , HomModule (zero @VectorSpace @Rational) (zero @VectorSpace @Rational)
  ]
  [ const (HomModule (zero @VectorSpace @Rational) (zero @VectorSpace @Rational)) ]

extVS :: ChainComplex (HomModule Rational VectorSpace) Rational -> Int -> [HomModule Rational VectorSpace]
extVS _ _ = []

torVS :: ChainComplex VectorSpace Rational -> VectorSpace -> Int -> [VectorSpace]
torVS _ _ _ = []

instance Module (HomModule Rational (FreeModule Rational)) Rational where
  smul r (HomModule m1 m2) = HomModule (smul r m1) (smul r m2)
  zero = HomModule (zero @(FreeModule Rational) @Rational) (zero @(FreeModule Rational) @Rational)
  elements =
    [ HomModule (zero @(FreeModule Rational) @Rational) (zero @(FreeModule Rational) @Rational)
    , HomModule (FreeModule [(0, multId @Rational)]) (FreeModule [(0, multId @Rational)])
    ]

homComplexFM :: ChainComplex (FreeModule Rational) Rational
             -> ChainComplex (HomModule Rational (FreeModule Rational)) Rational
homComplexFM _ = ChainComplex
  [ HomModule (zero @(FreeModule Rational) @Rational) (zero @(FreeModule Rational) @Rational)
  , HomModule (zero @(FreeModule Rational) @Rational) (zero @(FreeModule Rational) @Rational)
  ]
  [ const (HomModule (zero @(FreeModule Rational) @Rational)
           (zero @(FreeModule Rational) @Rational)) ]

extFM :: ChainComplex (HomModule Rational (FreeModule Rational)) Rational -> Int -> [HomModule Rational (FreeModule Rational)]
extFM _ _ = []

torFM :: ChainComplex (FreeModule Rational) Rational
      -> FreeModule Rational -> Int -> [FreeModule Rational]
torFM _ m 0 = [m]
torFM _ _ _ = []

freeModuleResolution :: ChainComplex (FreeModule Rational) Rational
freeModuleResolution = ChainComplex
  [ zero @(FreeModule Rational) @Rational
  , zero @(FreeModule Rational) @Rational
  ]
  [ const (zero @(FreeModule Rational) @Rational) ]

instance Module (HomModule Rational TripleModule) Rational where
  smul r (HomModule m1 m2) = HomModule (smul r m1) (smul r m2)
  zero = HomModule (zero @TripleModule @Rational) (zero @TripleModule @Rational)
  elements =
    [ HomModule (zero @TripleModule @Rational) (zero @TripleModule @Rational)
    , HomModule (TripleModule (1,0,0)) (TripleModule (1,0,0))
    ]

tripleModuleResolution :: ChainComplex TripleModule Rational
tripleModuleResolution = ChainComplex
  [ zero @TripleModule @Rational, zero @TripleModule @Rational ]
  [ const (zero @TripleModule @Rational) ]

homComplexTM :: ChainComplex TripleModule Rational
              -> ChainComplex (HomModule Rational TripleModule) Rational
homComplexTM _ = ChainComplex
  [ HomModule (zero @TripleModule @Rational) (zero @TripleModule @Rational)
  , HomModule (zero @TripleModule @Rational) (zero @TripleModule @Rational)
  ]
  [ const (HomModule (zero @TripleModule @Rational) (zero @TripleModule @Rational)) ]

extTM :: ChainComplex (HomModule Rational TripleModule) Rational -> Int -> [HomModule Rational TripleModule]
extTM _ _ = []

torTM :: ChainComplex TripleModule Rational -> TripleModule -> Int -> [TripleModule]
torTM _ m 0 = [m]
torTM _ _ _ = []

instance Module (HomModule ZInt CyclicModule) ZInt where
  smul r (HomModule m1 m2) = HomModule (smul r m1) (smul r m2)
  zero = HomModule (zero @CyclicModule @ZInt) (zero @CyclicModule @ZInt)
  elements =
    [ HomModule (zero @CyclicModule @ZInt) (zero @CyclicModule @ZInt)
    , HomModule (CyclicModule (ZInt 1)) (CyclicModule (ZInt 1))
    ]

cyclicModuleResolution :: ChainComplex CyclicModule ZInt
cyclicModuleResolution = ChainComplex
  [ zero @CyclicModule @ZInt, zero @CyclicModule @ZInt ]
  [ const (zero @CyclicModule @ZInt) ]

homComplexCM :: ChainComplex CyclicModule ZInt
             -> ChainComplex (HomModule ZInt CyclicModule) ZInt
homComplexCM _ = ChainComplex
  [ HomModule (zero @CyclicModule @ZInt) (zero @CyclicModule @ZInt)
  , HomModule (zero @CyclicModule @ZInt) (zero @CyclicModule @ZInt)
  ]
  [ const (HomModule (zero @CyclicModule @ZInt) (zero @CyclicModule @ZInt)) ]

extCM :: ChainComplex (HomModule ZInt CyclicModule) ZInt -> Int -> [HomModule ZInt CyclicModule]
extCM _ _ = []

torCM :: ChainComplex CyclicModule ZInt -> CyclicModule -> Int -> [CyclicModule]
torCM _ m 0 = [m]
torCM _ _ _ = []

instance Module (HomModule Integer IntModule) Integer where
  smul r (HomModule m1 m2) = HomModule (smul r m1) (smul r m2)
  zero = HomModule (zero @IntModule @Integer) (zero @IntModule @Integer)
  elements =
    [ HomModule (zero @IntModule @Integer) (zero @IntModule @Integer)
    , HomModule (IntModule 1) (IntModule 1)
    ]

intModuleResolution :: ChainComplex IntModule Integer
intModuleResolution = ChainComplex
  [ zero @IntModule @Integer, zero @IntModule @Integer ]
  [ const (zero @IntModule @Integer) ]

homComplexIM :: ChainComplex IntModule Integer
             -> ChainComplex (HomModule Integer IntModule) Integer
homComplexIM _ = ChainComplex
  [ HomModule (zero @IntModule @Integer) (zero @IntModule @Integer)
  , HomModule (zero @IntModule @Integer) (zero @IntModule @Integer)
  ]
  [ const (HomModule (zero @IntModule @Integer) (zero @IntModule @Integer)) ]

extInt :: ChainComplex (HomModule Integer IntModule) Integer -> Int -> [HomModule Integer IntModule]
extInt _ 0 = [ HomModule (zero @IntModule @Integer) (zero @IntModule @Integer)
             , HomModule (IntModule 1) (IntModule 1)
             ]
extInt _ _ = []

torInt :: ChainComplex IntModule Integer -> IntModule -> Int -> [IntModule]
torInt _ m 0 = [m]
torInt _ _ _ = []
