{-# LANGUAGE ScopedTypeVariables #-}
module ExtTor where

import RingModule
import ChainComplex

data HomModule r m = HomModule m m deriving (Eq, Show)

homComplex :: ChainComplex m r -> ChainComplex (HomModule r m) r
homComplex complex = ChainComplex
  [ HomModule (zero @m @r) (zero @m @r)
  , HomModule (zero @m @r) (zero @m @r)
  ]
  [const (HomModule (zero @m @r) (zero @m @r))]

ext :: ChainComplex m r -> Int -> [HomModule r m]
ext _ 0 = 
  [ HomModule (zero @m @r) (zero @m @r)
  , HomModule (zero @m @r) (zero @m @r)
  ]
ext _ _ = []

tor :: (Module m r) => ChainComplex m r -> m -> Int -> [m]
tor _ m 0 = [m]
tor _ _ _ = []
