{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}
module RingModule where

class Eq a => Ring a where
  addId :: a
  multId :: a
  add :: a -> a -> a
  mult :: a -> a -> a
  neg :: a -> a
  ringElements :: [a]

newtype Z2 = Z2 Int deriving (Eq, Show)

instance Ring Z2 where
  addId = Z2 0
  multId = Z2 1
  add (Z2 x) (Z2 y) = Z2 ((x + y) `mod` 2)
  mult (Z2 x) (Z2 y) = Z2 ((x * y) `mod` 2)
  neg (Z2 x) = Z2 ((-x) `mod` 2)
  ringElements = [Z2 0, Z2 1]

class (Ring r) => Module m r | m -> r where
  zero :: m
  addM :: m -> m -> m
  smul :: r -> m -> m
  negM :: m -> m
  moduleElements :: [m]

newtype Z2Module = Z2Module Z2 deriving (Eq, Show)

instance Module Z2Module Z2 where
  zero = Z2Module addId
  addM (Z2Module x) (Z2Module y) = Z2Module (add x y)
  smul r (Z2Module x) = Z2Module (mult r x)
  negM (Z2Module x) = Z2Module (neg x)
  moduleElements = [Z2Module (Z2 0), Z2Module (Z2 1)]

data FreeModule r = FreeModule r r deriving (Eq, Show)

instance Ring r => Module (FreeModule r) r where
  zero = FreeModule addId addId
  addM (FreeModule a1 b1) (FreeModule a2 b2) = 
    FreeModule (add a1 a2) (add b1 b2)
  smul s (FreeModule a b) = 
    FreeModule (mult s a) (mult s b)
  negM (FreeModule a b) = FreeModule (neg a) (neg b)
  moduleElements = [FreeModule x y | x <- ringElements, y <- ringElements]
