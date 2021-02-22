{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

-----------------------------------------------------------
-- Module      :  EPrelude
-- Copyright   :  (c) Matthew Marsland
-- License     :  BSD-style
--
-- Maintainer  :  marslandm@me.com
-- Status      :  work in progress
--
-- The EDPrelude: A version of Prelude built for students upwards in a flat file, supporting:
--  - Restricted Numeric Classes
--  - Intelligible Function Signatures
--  - Default Pretty-Printing
-----------------------------------------------------------

module EDPrelude (
    --EXPORT LIST
    --Numeric Types and Functions
    Num((+),(-),(*),negate,abs,signum,fromInteger),
    Real(toRational), 
    Fractional((/), recip),
    RealFrac(truncate,round,ceiling,floor),
    Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
    Integer, Double, Rational,
    (%), (^), (^^),
    even, odd,

    --Ordering Types and Functions
    Ord(compare,(<),(<=),(>),(>=),max,min),
    Ordering,

    --Equality and Booleans
    Eq((==),(/=)),
    Bool(True,False), 
    otherwise, (||), (&&),

    --Enum
    E.Enum,
    EDPrelude.toEnum, EDPrelude.fromEnum,

    --List Types and Functions
    length, take, drop, sum, product, (!!), zip, zipWith, unzip, isPrefixOf, map, elem,
    --Pair Functions
    fst, snd,

    --Char Types and Functions
    isDigit, isUpper, isLower, isAlpha, isAlphaNum, toUpper, toLower, digitToInteger, integerToDigit,

    --Showing as Strings, Characters, and Strings
    Show(..),
    Char, String,

    --Miscellaneous
    undefined, error, errorWithoutStackTrace, ($), (.),

    --Lifting and Monads
    Monad,
    (>>=), guard, return,
    liftM, liftM2
    ) where

--IMPORTS
--import GHC.Integer (Integer, plusInteger, minusInteger, timesInteger, negateInteger, absInteger, signumInteger)
import GHC.Num
import GHC.Real
import Data.Eq
import Data.Ord
import qualified GHC.Enum as E (Enum,toEnum,fromEnum)
import Data.Char ()
import GHC.Show
import GHC.Types (Char, Bool(True,False), Double)
import GHC.Base (String, error, errorWithoutStackTrace, ($), (.), undefined)
import Control.Monad (Monad, liftM, liftM2, (>>=), guard, return)

-- Num Functions

-- Bool Functions
otherwise :: Bool
otherwise = True

not :: Bool -> Bool
not True = False
not False = True

infixr 2 ||
(||) :: Bool -> Bool -> Bool
(||) False False    = False
(||) _ _            = True

infixr 3 &&
(&&) :: Bool -> Bool -> Bool
(&&) True True  = True
(&&) _ _        = False

-- Enum Functions
toEnum :: E.Enum a => Integer -> a
toEnum i = E.toEnum (fromIntegral i)

fromEnum :: E.Enum a => a -> Integer
fromEnum x = toInteger (E.fromEnum x)

-- Char Functions
isDigit :: Char -> Bool
isDigit c = c >= '0' && c <= '9'

isUpper :: Char -> Bool
isUpper c = c >= 'A' && c <= 'Z'

isLower :: Char -> Bool
isLower c = c >= 'a' && c <= 'z'

isAlpha :: Char -> Bool
isAlpha c = isUpper c || isLower c

isAlphaNum :: Char -> Bool
isAlphaNum c = isAlpha c || isDigit c

toUpper :: Char -> Char
toUpper c
    | isLower c = toEnum (fromEnum c - fromEnum 'a' + fromEnum 'A')
    | otherwise = c

toLower :: Char -> Char
toLower c
    | isUpper c = toEnum (fromEnum c - fromEnum 'A' + fromEnum 'a')
    | otherwise = c

digitToInteger :: Char -> Integer
digitToInteger c 
    | isDigit c = fromEnum c - fromEnum '0'
    | otherwise = errorWithoutStackTrace "Char.digitToInteger: not a digit."

integerToDigit :: Integer -> Char
integerToDigit i
    | i >= 0 && i <= 9 = toEnum (i + fromEnum '0')
    | otherwise = errorWithoutStackTrace "Char.integerToDigit: not a digit."

-- List Functions
(++) :: [a] -> [a] -> [a]
(++) [] y = y
(++) (x:xs) y = x : (++) xs y

length :: [a] -> Integer
length [] = 0
length (x:xs) = 1 + (length xs)

take :: Integer -> [a] -> [a]
take n [] = []
take 0 (x:xs) = []
take n (x:xs) = x : take (n-1) xs

drop :: Integer -> [a] -> [a]
drop n [] = []
drop 0 xs = xs
drop n (x:xs) = drop (n-1) xs

sum :: Num a => [a] -> a
sum [] = 0
sum (x:xs) = x + sum xs

product :: Num a => [a] -> a
product [] = 1
product (x:xs) = x * product xs

infixl 9 !!
(!!) :: [a] -> Integer -> a
xs     !! n | n < 0 =  errorWithoutStackTrace "EDPrelude.!!: negative index"
[]     !! _         =  errorWithoutStackTrace "EDPrelude.!!: index too large"
(x:_)  !! 0         =  x
(_:xs) !! n         =  xs !! (n-1)

map :: (a -> b) -> [a] -> [b]
map _ [] = []
map f (a:as) = (f a) : (map f as)

fst :: (a,b) -> a
fst (a,b) = a

snd :: (a,b) -> b
snd (a,b) = b

zip :: [a] -> [b] -> [(a,b)]
zip _ [] = []
zip [] _ = []
zip (a:as) (b:bs) = (a,b) : (zip as bs)

zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith f _ [] = []
zipWith f [] _ = []
zipWith f (a:as) (b:bs) = (f a b) : (zipWith f as bs)

unzip :: [(a,b)] -> ([a],[b])
unzip abs = (as, bs)
    where
        as = map fst abs
        bs = map snd abs

isPrefixOf :: (Eq a) => [a] -> [a] -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (x:xs) (y:ys) = x == y && isPrefixOf xs ys

elem :: (Eq a) => a -> [a] -> Bool
elem x [] = False
elem x (y:ys) = x == y || elem x ys

-- Undefined