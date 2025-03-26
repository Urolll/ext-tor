{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module Hom where

import RingModule
import ChainComplex

data HomModule r n = HomModule n n deriving (Eq, Show)

instance (Ring r, Module n r, Eq n) => Module (HomModule r n) r where
  zero = HomModule zero zero
  addM (HomModule x1 y1) (HomModule x2 y2) =
    HomModule (addM x1 x2) (addM y1 y2)
  smul s (HomModule x y) =
    HomModule (smul s x) (smul s y)
  negM (HomModule x y) = HomModule (negM x) (negM y)
  moduleElements = [ HomModule a b | a <- moduleElements, b <- moduleElements, a == zero]

data CochainComplex m r where
  CochainComplex ::
    { hom0 :: m
    , hom1 :: m
    , coboundary :: m -> m
    } -> CochainComplex m r



homComplex :: forall r n. (Ring r, Module n r, Eq n)
           => ChainComplex (FreeModule r) r
           -> CochainComplex (HomModule r n) r
homComplex cc = CochainComplex
  { hom0 = zero
  , hom1 = zero
  , coboundary = homDiff (diff cc)
  }
  where
    homDiff :: (FreeModule r -> FreeModule r)
            -> HomModule r n
            -> HomModule r n
    homDiff d hom@(HomModule x y) =
      if module1 cc == zero
      then zero
      else case d (FreeModule multId addId) of
        FreeModule a b -> HomModule (smul a x `addM` smul b y) zero

verifyCochainComplex :: (Module m r, Eq m) => CochainComplex m r -> Bool
verifyCochainComplex cc = all isZero [coboundary cc (coboundary cc x) | x <- basis]
  where
    basis = [zero, smul multId zero]
    isZero x = x == zero
