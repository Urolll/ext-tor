{-# LANGUAGE InstanceSigs, FlexibleInstances, AllowAmbiguousTypes #-}
module RingModule where

class Ring r where
  addId :: r
  multId :: r
  add :: r -> r -> r
  neg :: r -> r
  mult :: r -> r -> r

data Z2 = Z2 Int deriving (Eq, Show)

instance Ring Z2 where
  addId = Z2 0
  multId = Z2 1
  add (Z2 a) (Z2 b) = Z2 ((a + b) `mod` 2)
  neg (Z2 a) = Z2 ((2 - a) `mod` 2)
  mult (Z2 a) (Z2 b) = Z2 ((a * b) `mod` 2)

class (Ring r) => Module m r where
  smul :: r -> m -> m
  zero :: m
  elements :: [m]

newtype FreeModule r = FreeModule [(Int, r)] deriving (Eq, Show)

instance (Ring r) => Module (FreeModule r) r where
  smul r (FreeModule xs) = FreeModule [(i, r `mult` x) | (i, x) <- xs]
  zero = FreeModule []
  elements = [FreeModule [], FreeModule [(0, multId)]]

newtype Z2Module = Z2Module Z2 deriving (Eq, Show)

instance Module Z2Module Z2 where
  smul r (Z2Module x) = Z2Module (r `mult` x)
  zero = Z2Module addId
  elements = [Z2Module (Z2 0), Z2Module (Z2 1)]

z2Module :: Z2Module
z2Module = Z2Module (Z2 1)
