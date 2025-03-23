{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FunctionalDependencies, GADTs #-}
module RingModule where

class Eq a => Ring a where
  addId :: a
  multId :: a
  add :: a -> a -> a
  mult :: a -> a -> a
  neg :: a -> a

newtype Z2 = Z2 Int deriving (Eq, Show)

instance Ring Z2 where
  addId = Z2 0
  multId = Z2 1
  add (Z2 x) (Z2 y) = Z2 ((x + y) `mod` 2)
  mult (Z2 x) (Z2 y) = Z2 ((x * y) `mod` 2)
  neg (Z2 x) = Z2 ((-x) `mod` 2)

class (Ring r) => Module m r | m -> r where
  zero :: m
  addM :: m -> m -> m
  smul :: r -> m -> m

newtype Z2Module = Z2Module Z2 deriving (Eq, Show)

instance Module Z2Module Z2 where
  zero = Z2Module addId
  addM (Z2Module x) (Z2Module y) = Z2Module (add x y)
  smul r (Z2Module x) = Z2Module (mult r x)

data FreeModule r = FreeModule r r deriving (Eq, Show)

instance Ring r => Module (FreeModule r) r where
  zero = FreeModule addId addId
  addM (FreeModule a1 b1) (FreeModule a2 b2) = 
    FreeModule (add a1 a2) (add b1 b2)
  smul s (FreeModule a b) = 
    FreeModule (mult s a) (mult s b)

data ChainComplex r where
  ChainComplex ::
    { moduleN   :: FreeModule r
    , moduleNm1 :: FreeModule r
    , diff      :: FreeModule r -> FreeModule r
    } -> ChainComplex r

z2Resolution :: ChainComplex Z2
z2Resolution = ChainComplex
  { moduleN = FreeModule (Z2 1) (Z2 1)
  , moduleNm1 = FreeModule (Z2 1) (Z2 1)
  , diff = \(FreeModule a b) -> FreeModule (add a a) (add b b)
  }

verifyComplex :: Ring r => ChainComplex r -> Bool
verifyComplex cc = all isZero [diff cc (diff cc x) | x <- basis]
  where
    basis = [FreeModule addId addId, FreeModule addId multId]
    isZero (FreeModule a b) = a == addId && b == addId
