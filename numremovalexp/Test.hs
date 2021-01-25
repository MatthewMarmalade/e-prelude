module Test () where
import PrettierPrelude

--test1 :: Int -> Int
--test1 x = x + 1         --Breaks, no Ints exist!

test2 :: [Integer] -> Integer
test2 xs = (length xs) + 1        --Should break at first then work as length is redefined on Integer

data Tree = Leaf Integer | Node Tree Tree deriving (Generic, Out)

test3 = Leaf 1
test4 = Node (Node (Leaf 123456789) (Node (Leaf 87654432) (Leaf 134967859485135))) (Leaf 135879135315)