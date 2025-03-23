{-# LANGUAGE TypeApplications #-}
module Main where

import Test.HUnit
import RingModule
import ChainComplex
import Hom

main :: IO ()
main = do
  otter <- runTestTT $ TestList [testRing, testModule, testChainComplex, testHomComplex]
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
