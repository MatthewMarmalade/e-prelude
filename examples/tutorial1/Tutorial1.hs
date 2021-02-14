module Tutorial1 where

import EPrelude
import PicturesSVG -- needed for the optional chess part
import Test.QuickCheck


-- 2.
double :: Integer -> Integer
double x = x + x

square :: Integer -> Integer
square x = undefined

-- 3.
isTriple :: Integer -> Integer -> Integer -> Bool
isTriple a b c = undefined

-- 4.
leg1 :: Integer -> Integer -> Integer
leg1 x y = undefined

leg2 :: Integer -> Integer -> Integer
leg2 x y = undefined

hyp :: Integer -> Integer -> Integer
hyp x y = undefined

-- 5.
prop_triple :: Integer -> Integer -> Bool
prop_triple x y = isTriple (leg1 x y) (leg2 x y) (hyp x y)

-- 7.
pic1 :: Picture
pic1 = undefined

pic2 :: Picture
pic2 = undefined

-- ** Functions

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)

-- 8.
twoAbove :: Picture -> Picture
twoAbove x = undefined

fourPictures :: Picture -> Picture
fourPictures x = undefined

-- 9.
-- a)
emptyRow :: Picture
emptyRow = undefined

-- b)
otherEmptyRow :: Picture
otherEmptyRow = undefined

-- c)
middleBoard :: Picture
middleBoard = undefined

-- d)
whiteRow :: Picture
whiteRow = undefined

blackRow :: Picture
blackRow = undefined

-- e)
populatedBoard :: Picture
populatedBoard = undefined
