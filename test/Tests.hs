{-# LANGUAGE TypeApplications #-}
module Main where

import Test.HUnit
import RingModule
import ChainComplex
import Hom
import ExtTor

main :: IO ()
main = do
  otter <- runTestTT $ TestList [testRing, testModule, testChainComplex, testHomComplex, testExt]
  return ()

testRing :: Test
testRing = TestList [
  "Additive Identity" ~: addId @Z2 ~?= Z2 0,
  "Additive Inverse" ~: neg (Z2 1) ~?= Z2 1,
  "Multiplication" ~: mult (Z2 1) (Z2 1) ~?= Z2 1
  ]

testModule :: Test
testModule = TestList [
  "Zero Element" ~: zero @Z2Module @Z2 ~?= Z2Module (Z2 0),
  "Scalar Multiplication" ~: smul (Z2 1) (Z2Module (Z2 1)) ~?= Z2Module (Z2 1)
  ]

testChainComplex :: Test
testChainComplex = TestList [
  "d^2 = 0" ~: verifyComplex z2Resolution ~?= True
  ]

testHomComplex :: Test
testHomComplex = TestCase $ do
  let complex :: CochainComplex (HomModule Z2 Z2Module) Z2
      complex = homComplex z2Resolution
  assertBool "delta^2 = 0" (verifyCochainComplex complex)

testExt :: Test
testExt = TestCase $ do
  let complex :: CochainComplex (HomModule Z2 Z2Module) Z2
      complex = homComplex z2Resolution
      ext0 = ext complex 0
      ext1 = ext complex 1
  
  assertEqual "Ext^0 should have 2 elements" 2 (length ext0)
  
  assertEqual "Ext^1 should be trivial" 0 (length ext1)
  
  assertBool "Identity morphism present" 
    (HomModule (Z2Module (Z2 1)) (Z2Module (Z2 1)) `elem` ext0)
  assertBool "Zero morphism present"
    (HomModule (Z2Module (Z2 0)) (Z2Module (Z2 0)) `elem` ext0)

