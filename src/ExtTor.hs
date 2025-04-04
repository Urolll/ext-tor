{-# LANGUAGE ScopedTypeVariables, TypeApplications, FlexibleContexts #-}
module ExtTor where

import RingModule
import ChainComplex

data HomModule r m = HomModule m m deriving (Eq, Show)

instance Module (HomModule Z2 Z2Module) Z2 where
  smul r (HomModule m1 m2) = HomModule (smul r m1) (smul r m2)
  zero = HomModule (zero @Z2Module @Z2) (zero @Z2Module @Z2)
  elements = [ HomModule (zero @Z2Module @Z2) (zero @Z2Module @Z2)
             , HomModule (Z2Module (Z2 1)) (Z2Module (Z2 1))
             ]

homComplex :: ChainComplex Z2Module Z2 -> ChainComplex (HomModule Z2 Z2Module) Z2
homComplex _ = ChainComplex
  [ HomModule (zero @Z2Module @Z2) (zero @Z2Module @Z2)
  , HomModule (zero @Z2Module @Z2) (zero @Z2Module @Z2)
  ]
  [ const (HomModule (zero @Z2Module @Z2) (zero @Z2Module @Z2)) ]

ext :: ChainComplex (HomModule Z2 Z2Module) Z2 -> Int -> [HomModule Z2 Z2Module]
ext _ 0 = 
  [ HomModule (zero @Z2Module @Z2) (zero @Z2Module @Z2)
  , HomModule (Z2Module (Z2 1)) (Z2Module (Z2 1))
  ]
ext _ _ = []

tor :: ChainComplex Z2Module Z2 -> Z2Module -> Int -> [Z2Module]
tor _ m 0 = [m]
tor _ _ _ = []
