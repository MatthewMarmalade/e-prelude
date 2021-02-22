module Tutorial2 where

import EDPrelude
--import Data.Char
--import Data.List
--import Test.QuickCheck
--import Control.Monad (guard)


-- 1. halveEvens

-- List-comprehension version
halveEvens :: [Integer] -> [Integer]
halveEvens xs = undefined

-- This is for testing only. Do not try to understand this (yet).
halveEvensReference :: [Integer] -> [Integer]
halveEvensReference = (>>= \x -> guard (x `mod` 2 == 0) >>= \_ -> return $ x `div` 2)

-- Mutual test
prop_halveEvens :: [Integer] -> Bool
prop_halveEvens xs = undefined


-- 2. inRange

-- List-comprehension version
inRange :: Integer -> Integer -> [Integer] -> [Integer]
inRange lo hi xs = undefined


-- 3. countPositives

-- List-comprehension version
countPositives :: [Integer] -> Integer
countPositives list = undefined


-- 4. multDigits

-- List-comprehension version
multDigits :: String -> Integer
multDigits str = undefined

countDigits :: String -> Integer
countDigits str = undefined

prop_multDigits :: String -> Bool
prop_multDigits xs = undefined


-- 5. capitalise

-- List-comprehension version
capitalise :: String -> String
capitalise s = undefined


-- 6. title

lowercase :: String -> String
lowercase xs = undefined

-- List-comprehension version
title :: [String] -> [String]
title _ = undefined


-- 7. signs

sign :: Integer -> Char
sign i = undefined

signs :: [Integer] -> String
signs xs = undefined


-- 8. score

score :: Char -> Integer
score x  = undefined

totalScore :: String -> Integer
totalScore xs = undefined

prop_totalScore_pos :: String -> Bool
prop_totalScore_pos xs = undefined


-- 9. pennypincher

-- List-comprehension version.
pennypincher :: [Integer] -> Integer
pennypincher prices = undefined

-- And the test itself
prop_pennypincher :: [Integer] -> Bool
prop_pennypincher xs = undefined


-- ** Optional Material

-- 10. crosswordFind

-- List-comprehension version
crosswordFind :: Char -> Integer -> Integer -> [String] -> [String]
crosswordFind letter pos len words = undefined


-- 11. search

-- List-comprehension version
search :: String -> Char -> [Integer]
search str goal = undefined

-- Depending on the property you want to test, you might want to change the type signature
prop_search :: String -> Char -> Bool
prop_search str goal = undefined


-- 12. contains

contains :: String -> String -> Bool
contains str substr = undefined

-- Depending on the property you want to test, you might want to change the type signature
prop_contains :: String -> String -> Bool
prop_contains str1 str2 = undefined