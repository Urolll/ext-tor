{-# LANGUAGE GADTs, FlexibleInstances, MultiParamTypeClasses, ScopedTypeVariables #-}
module Hom where

import RingModule
import ChainComplex

data HomModule r n = HomModule n n deriving (Eq, Show)

instance (Ring r, Module n r) => Module (HomModule r n) r where
  zero = HomModule zero zero
  addM (HomModule x1 y1) (HomModule x2 y2) =
    HomModule (addM x1 x2) (addM y1 y2)
  smul s (HomModule x y) =
    HomModule (smul s x) (smul s y)

data CochainComplex m r where
  CochainComplex ::
    { homModuleN   :: m
    , homModuleNp1 :: m
    , coboundary   :: m -> m
    } -> CochainComplex m r

homComplex :: forall r n. (Ring r, Module n r)
           => ChainComplex (FreeModule r) r
           -> CochainComplex (HomModule r n) r
homComplex cc = CochainComplex
  { homModuleN = HomModule zero zero
  , homModuleNp1 = HomModule zero zero
  , coboundary = homDiff (diff cc)
  }
  where
    homDiff :: (FreeModule r -> FreeModule r)
            -> HomModule r n
            -> HomModule r n
    homDiff d (HomModule x y) =
      let d1 = d (FreeModule addId addId)
          d2 = d (FreeModule addId multId)
          newA = case d1 of
            FreeModule a b -> addM (smul a x) (smul b y)
          newB = case d2 of
            FreeModule a b -> addM (smul a x) (smul b y)
      in HomModule newA newB

verifyCochainComplex :: (Module m r, Eq m) => CochainComplex m r -> Bool
verifyCochainComplex cc = all isZero [coboundary cc (coboundary cc x) | x <- basis]
  where
    basis = [zero, smul multId zero]
    isZero x = x == zero
