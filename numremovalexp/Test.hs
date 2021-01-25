{-# LANGUAGE NoImplicitPrelude #-}

module Test () where
import PrettierPrelude

--test1 :: Int -> Int
--test1 x = x + 1         --Should work at first then break as there are no Ints

test2 :: [Integer] -> Integer
test2 xs = (length xs) + 1        --Should break at first then work as length is redefined on Integer