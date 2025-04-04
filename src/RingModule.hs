{-# LANGUAGE InstanceSigs, FlexibleInstances, AllowAmbiguousTypes #-}
module RingModule where

import Data.Ratio ()

class Ring r where
  addId    :: r
  multId   :: r
  add      :: r -> r -> r
  neg      :: r -> r
  mult     :: r -> r -> r
  ringSize :: Maybe Integer
  ringSize = Nothing

data Z2 = Z2 Int deriving (Eq, Show)

instance Ring Z2 where
  addId    = Z2 0
  multId   = Z2 1
  add (Z2 a) (Z2 b) = Z2 ((a + b) `mod` 2)
  neg (Z2 a) = Z2 ((2 - a) `mod` 2)
  mult (Z2 a) (Z2 b) = Z2 ((a * b) `mod` 2)
  ringSize = Just 2

instance Ring Rational where
  addId    = 0
  multId   = 1
  add      = (+)
  neg      = negate
  mult     = (*)
  ringSize = Nothing

instance Ring Integer where
  addId    = 0
  multId   = 1
  add      = (+)
  neg      = negate
  mult     = (*)
  ringSize = Nothing

newtype ZInt = ZInt Integer deriving (Eq, Show)

instance Ring ZInt where
  addId    = ZInt 0
  multId   = ZInt 1
  add (ZInt a) (ZInt b) = ZInt (a + b)
  neg (ZInt a) = ZInt (-a)
  mult (ZInt a) (ZInt b) = ZInt (a * b)
  ringSize = Nothing

class (Ring r) => Module m r where
  smul     :: r -> m -> m
  zero     :: m
  elements :: [m]

newtype FreeModule r = FreeModule [(Int, r)] deriving (Eq, Show)

instance (Ring r) => Module (FreeModule r) r where
  smul r (FreeModule xs) = FreeModule [ (i, r `mult` x) | (i, x) <- xs ]
  zero = FreeModule []
  elements = [FreeModule [], FreeModule [(0, multId)]]

newtype Z2Module = Z2Module Z2 deriving (Eq, Show)

instance Module Z2Module Z2 where
  smul r (Z2Module x) = Z2Module (r `mult` x)
  zero = Z2Module addId
  elements = [Z2Module (Z2 0), Z2Module (Z2 1)]

z2Module :: Z2Module
z2Module = Z2Module (Z2 1)

newtype VectorSpace = VectorSpace [Rational] deriving (Eq, Show)

instance Module VectorSpace Rational where
  smul r (VectorSpace xs) = VectorSpace (map (r *) xs)
  zero = VectorSpace [0,0]
  elements =
    [ VectorSpace [0,0]
    , VectorSpace [1,0]
    , VectorSpace [0,1]
    , VectorSpace [1,1]
    ]

newtype TripleModule = TripleModule (Rational, Rational, Rational) deriving (Eq, Show)

instance Module TripleModule Rational where
  smul r (TripleModule (a, b, c)) = TripleModule (r * a, r * b, r * c)
  zero = TripleModule (0, 0, 0)
  elements =
    [ TripleModule (0, 0, 0)
    , TripleModule (1, 0, 0)
    , TripleModule (0, 1, 0)
    , TripleModule (0, 0, 1)
    , TripleModule (1, 1, 1)
    ]

newtype CyclicModule = CyclicModule ZInt deriving (Eq, Show)

instance Module CyclicModule ZInt where
  smul (ZInt r) (CyclicModule (ZInt a)) = CyclicModule (ZInt ((r * a) `mod` 3))
  zero = CyclicModule (ZInt 0)
  elements = [ CyclicModule (ZInt x) | x <- [0, 1, 2] ]

newtype IntModule = IntModule Integer deriving (Eq, Show)

instance Module IntModule Integer where
  smul r (IntModule a) = IntModule (r * a)
  zero = IntModule 0
  elements = [IntModule 0, IntModule 1, IntModule (-1)]
