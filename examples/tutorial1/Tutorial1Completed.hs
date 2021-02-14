module Tutorial1Completed where

import EPrelude
import PicturesSVG -- needed for the optional chess part
import Test.QuickCheck


-- 2.
double :: Integer -> Integer
double x = x + x

square :: Integer -> Integer
square x = x * x

-- 3.
isTriple :: Integer -> Integer -> Integer -> Bool
isTriple a b c = square a + square b == square c

-- 4.
leg1 :: Integer -> Integer -> Integer
leg1 x y = square x - square y

leg2 :: Integer -> Integer -> Integer
leg2 x y = 2 * y * x

hyp :: Integer -> Integer -> Integer
hyp x y = square x + square y

-- 5.
prop_triple :: Integer -> Integer -> Bool
prop_triple x y = isTriple (leg1 x y) (leg2 x y) (hyp x y)

-- 7.
pic1 :: Picture
pic1 = above (beside knight (invert knight)) (beside (invert knight) knight)

pic2 :: Picture
pic2 = above (beside knight (invert knight)) (flipV (beside knight (invert knight)))

-- ** Functions

twoBeside :: Picture -> Picture
twoBeside x = beside x (invert x)

-- 8.
twoAbove :: Picture -> Picture
twoAbove x = above x (invert x)

fourPictures :: Picture -> Picture
fourPictures x = twoAbove (twoBeside x)

-- 9.
-- a)
emptyRow :: Picture
emptyRow = repeatH 4 (beside whiteSquare blackSquare)

-- b)
otherEmptyRow :: Picture
otherEmptyRow = repeatH 4 (beside blackSquare whiteSquare)

-- c)
middleBoard :: Picture
middleBoard = above (above emptyRow otherEmptyRow) (above emptyRow otherEmptyRow)

-- d)
whiteRow :: Picture
whiteRow = over (beside (beside (beside rook knight) (beside bishop queen)) (beside (beside king bishop) (beside knight rook))) otherEmptyRow

blackRow :: Picture
blackRow = over (invert (beside (beside (beside rook knight) (beside bishop queen)) (beside (beside king bishop) (beside knight rook)))) emptyRow

-- e)
whitePawns :: Picture
whitePawns = over (repeatH 8 pawn) emptyRow

blackPawns :: Picture
blackPawns = over (repeatH 8 (invert pawn)) otherEmptyRow

populatedBoard :: Picture
populatedBoard = above (above blackRow blackPawns) (above middleBoard (above whitePawns whiteRow))
