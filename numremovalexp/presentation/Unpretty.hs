{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
module Unpretty where

import Text.PrettyPrint.GenericPretty

data Tree = Leaf Integer | Node Tree Tree deriving (Generic, Out)

smallTree = Leaf 1

largeTree = Node 
                (Node 
                    (Leaf 123456789) 
                    (Node 
                        (Leaf 87654432) 
                        (Leaf 134967859485135))) 
                (Leaf 135879135315)

veryLargeTree = Node largeTree (Node largeTree largeTree)

{-pretty :: Tree -> Doc
pretty (Leaf a)   = parens $ text "Leaf" <+> integer a
pretty (Node a b) = parens $ text "Node" $$ nest 1 (pretty a)
                                         $$ nest 1 (pretty b)
-}
pretty' :: Tree -> IO ()
pretty' t = ppLen 40 t