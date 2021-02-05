module Test () where
import EPrelude

testLength :: [Integer] -> Integer
testLength xs = (length xs) + 1

data Tree = Leaf Integer | Node Tree Tree deriving (Generic, Out, Show)

testSmallTree = Leaf 1
testLargeTree = Node (Node (Leaf 123456789) (Node (Leaf 87654432) (Leaf 134967859485135))) (Leaf 135879135315)
testVeryLargeTree = Node testLargeTree (Node testLargeTree testLargeTree)

alphabet :: String
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

testTakeAndDrop :: Integer -> String
testTakeAndDrop x = drop (x - 1) (take x alphabet)

testIndex :: Integer -> Char
testIndex x = alphabet !! x

data Season = Summer | Fall | Winter | Spring deriving (Enum, Show)

testFromEnum :: Integer
testFromEnum = fromEnum Fall

i1 = 1 :: Integer

testToEnum :: Season
testToEnum = toEnum 1