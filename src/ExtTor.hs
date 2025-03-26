module ExtTor where

import RingModule
import Hom

ext :: (Module m r, Eq m) => CochainComplex m r -> Int -> [m]
ext cc n
  | n == 0    = [x | x <- moduleElements, coboundary cc x == zero]
  | n == 1    = [x | x <- moduleElements, x `notElem` image]
  | otherwise = []
  where
    image = [coboundary cc y | y <- moduleElements]
