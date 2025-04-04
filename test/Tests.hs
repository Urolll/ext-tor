{-# LANGUAGE TypeApplications #-}
module Main where

import Test.HUnit
import RingModule
import ChainComplex
import ExtTor

main :: IO ()
main = do
  otter <- runTestTT $ TestList
    [ testRing, testModule, testChainComplex, testHomComplex, testExt
    , testVectorSpace, testHomComplexVS, testExtVS, testTorVS
    , testFreeModule, testHomComplexFM, testExtFM, testTorFM
    , testTripleModule, testHomComplexTM, testExtTM, testTorTM
    , testCyclicModule, testHomComplexCM, testExtCM, testTorCM
    , testIntModule, testHomComplexIM, testExtInt, testTorInt
    ]
  return ()

testRing :: Test
testRing = TestList
  [ "Additive Identity" ~: addId @Z2 ~?= Z2 0
  , "Additive Inverse" ~: neg (Z2 1) ~?= Z2 1
  , "Multiplication" ~: mult (Z2 1) (Z2 1) ~?= Z2 1
  ]

testModule :: Test
testModule = TestList
  [ "Zero Element" ~: zero @Z2Module @Z2 ~?= Z2Module (Z2 0)
  , "Scalar Multiplication" ~: smul (Z2 1) (Z2Module (Z2 1)) ~?= Z2Module (Z2 1)
  ]

testChainComplex :: Test
testChainComplex = TestList
  [ "d^2 = 0" ~: verifyComplex z2Resolution ~?= True
  ]

testHomComplex :: Test
testHomComplex = TestCase $ do
  let complex = homComplex z2Resolution
  assertBool "delta^2 = 0" (verifyComplex complex)

testExt :: Test
testExt = TestCase $ do
  let complex = homComplex z2Resolution
      ext0 = ext complex 0
      ext1 = ext complex 1
  assertEqual "Ext^0 should have 2 elements" 2 (length ext0)
  assertEqual "Ext^1 should be trivial" 0 (length ext1)
  assertBool "Identity morphism present" (HomModule (Z2Module (Z2 1)) (Z2Module (Z2 1)) `elem` ext0)
  assertBool "Zero morphism present" (HomModule (Z2Module (Z2 0)) (Z2Module (Z2 0)) `elem` ext0)

testVectorSpace :: Test
testVectorSpace = TestList
  [ "VectorSpace: d^2 = 0" ~: verifyComplex vectorSpaceResolution ~?= True
  ]

testHomComplexVS :: Test
testHomComplexVS = TestCase $ do
  let complex = homComplexVS vectorSpaceResolution
  assertBool "VectorSpace homComplex delta^2 = 0" (verifyComplex complex)

testExtVS :: Test
testExtVS = TestCase $ do
  let complex = homComplexVS vectorSpaceResolution
      extResult = extVS complex 0
  assertEqual "Ext^0 for vector space should be trivial" 0 (length extResult)

testTorVS :: Test
testTorVS = TestCase $ do
  let torResult = torVS vectorSpaceResolution (VectorSpace [1,0]) 0
  assertEqual "Tor^0 for vector space should be trivial" 0 (length torResult)

testFreeModule :: Test
testFreeModule = TestList
  [ "FreeModule: d^2 = 0" ~: verifyComplex freeModuleResolution ~?= True
  ]

testHomComplexFM :: Test
testHomComplexFM = TestCase $ do
  let complex = homComplexFM freeModuleResolution
  assertBool "FreeModule homComplex delta^2 = 0" (verifyComplex complex)

testExtFM :: Test
testExtFM = TestCase $ do
  let complex = homComplexFM freeModuleResolution
      extResult = extFM complex 0
  assertEqual "Ext^0 for free module should be trivial" 0 (length extResult)

testTorFM :: Test
testTorFM = TestCase $ do
  let torResult = torFM freeModuleResolution (FreeModule [(0, multId)]) 0
  assertEqual "Tor^0 for free module should return the module" 1 (length torResult)

testTripleModule :: Test
testTripleModule = TestList
  [ "TripleModule: d^2 = 0" ~: verifyComplex tripleModuleResolution ~?= True
  ]

testHomComplexTM :: Test
testHomComplexTM = TestCase $ do
  let complex = homComplexTM tripleModuleResolution
  assertBool "TripleModule homComplex delta^2 = 0" (verifyComplex complex)

testExtTM :: Test
testExtTM = TestCase $ do
  let complex = homComplexTM tripleModuleResolution
      extResult = extTM complex 0
  assertEqual "Ext^0 for triple module should be trivial" 0 (length extResult)

testTorTM :: Test
testTorTM = TestCase $ do
  let torResult = torTM tripleModuleResolution (TripleModule (1,1,1)) 0
  assertEqual "Tor^0 for triple module should return the module" 1 (length torResult)

testCyclicModule :: Test
testCyclicModule = TestList
  [ "CyclicModule: d^2 = 0" ~: verifyComplex cyclicModuleResolution ~?= True
  ]

testHomComplexCM :: Test
testHomComplexCM = TestCase $ do
  let complex = homComplexCM cyclicModuleResolution
  assertBool "CyclicModule homComplex delta^2 = 0" (verifyComplex complex)

testExtCM :: Test
testExtCM = TestCase $ do
  let complex = homComplexCM cyclicModuleResolution
      extResult = extCM complex 0
  assertEqual "Ext^0 for cyclic module should be trivial" 0 (length extResult)

testTorCM :: Test
testTorCM = TestCase $ do
  let torResult = torCM cyclicModuleResolution (CyclicModule (ZInt 1)) 0
  assertEqual "Tor^0 for cyclic module should return the module" 1 (length torResult)

testIntModule :: Test
testIntModule = TestList
  [ "IntModule: d^2 = 0" ~: verifyComplex intModuleResolution ~?= True
  ]

testHomComplexIM :: Test
testHomComplexIM = TestCase $ do
  let complex = homComplexIM intModuleResolution
  assertBool "IntModule homComplex delta^2 = 0" (verifyComplex complex)

testExtInt :: Test
testExtInt = TestCase $ do
  let complex = homComplexIM intModuleResolution
      extResult = extInt complex 0
  assertEqual "Ext^0 for int module should have 2 elements" 2 (length extResult)

testTorInt :: Test
testTorInt = TestCase $ do
  let torResult = torInt intModuleResolution (IntModule 1) 0
  assertEqual "Tor^0 for int module should return the module" 1 (length torResult)
