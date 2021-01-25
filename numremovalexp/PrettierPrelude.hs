{-# LANGUAGE Trustworthy #-}
{-# LANGUAGE NoImplicitPrelude #-}

-----------------------------------------------------------
-- Module      :  PrettierPrelude
-- Copyright   :  (c) Matthew Marsland
-- License     :  BSD-style
--
-- Maintainer  :  marslandm@me.com
-- Status      :  work in progress
--
-- The PrettierPrelude: A modification of Prelude specifically restricting Numeric classes,
-- and altering type signatures accordingly.
-----------------------------------------------------------

module PrettierPrelude (

    -- # Standard types, classes and related functions

    -- ## Basic data types
    Bool(False, True),
    (&&), (||), not, otherwise,

    Maybe(Nothing, Just),
    maybe,

    Either(Left, Right),
    either,

    Ordering(LT, EQ, GT),
    Char, String,

    -- ### Tuples
    fst, snd, curry, uncurry,

    -- ## Basic type classes
    Eq((==), (/=)),
    Ord(compare, (<), (<=), (>=), (>), max, min),
    Enum(succ, pred, toEnum, fromEnum, enumFrom, enumFromThen,
         enumFromTo, enumFromThenTo),
    Bounded(minBound, maxBound),

    -- ## Numbers

    -- ### Numeric types
    Integer, Float, Double, Rational,
    --Int, Integer, Float, Double,
    --Rational, Word,

    -- ### Numeric type classes
    Num((+), (-), (*), negate, abs, signum, fromInteger),
    Real(toRational),
    Integral(quot, rem, div, mod, quotRem, divMod, toInteger),
    Fractional((/), recip, fromRational),
    Floating(pi, exp, log, sqrt, (**), logBase, sin, cos, tan,
             asin, acos, atan, sinh, cosh, tanh, asinh, acosh, atanh),
    RealFrac(properFraction, truncate, round, ceiling, floor),
    RealFloat(floatRadix, floatDigits, floatRange, decodeFloat,
              encodeFloat, exponent, significand, scaleFloat, isNaN,
              isInfinite, isDenormalized, isIEEE, isNegativeZero, atan2),

    -- ### Numeric functions
    subtract, even, odd, gcd, lcm, (^), (^^),
    fromIntegral, realToFrac,

    -- ## Semigroups and Monoids
    Semigroup((<>)),
    Monoid(mempty, mappend, mconcat),

    -- ## Monads and functors
    Functor(fmap, (<$)), (<$>),
    Applicative(pure, (<*>), (*>), (<*)),
    Monad((>>=), (>>), return),
    MonadFail(fail),
    mapM_, sequence_, (=<<),

    -- ## Folds and traversals
    Foldable(elem,      -- :: (Foldable t, Eq a) => a -> t a -> Bool
             -- fold,   -- :: Monoid m => t m -> m
             foldMap,   -- :: Monoid m => (a -> m) -> t a -> m
             foldr,     -- :: (a -> b -> b) -> b -> t a -> b
             -- foldr', -- :: (a -> b -> b) -> b -> t a -> b
             foldl,     -- :: (b -> a -> b) -> b -> t a -> b
             -- foldl', -- :: (b -> a -> b) -> b -> t a -> b
             foldr1,    -- :: (a -> a -> a) -> t a -> a
             foldl1,    -- :: (a -> a -> a) -> t a -> a
             maximum,   -- :: (Foldable t, Ord a) => t a -> a
             minimum,   -- :: (Foldable t, Ord a) => t a -> a
             product,   -- :: (Foldable t, Num a) => t a -> a
             sum),      -- :: Num a => t a -> a
             -- toList) -- :: Foldable t => t a -> [a]

    Traversable(traverse, sequenceA, mapM, sequence),

    -- ## Miscellaneous functions
    id, const, (.), flip, ($), until,
    asTypeOf, error, errorWithoutStackTrace, undefined,
    seq, ($!),

    -- # List operations
    List.map, (List.++), List.filter,
    List.head, List.last, List.tail, List.init, (List.!!),
    Foldable.null, PrettierPrelude.length,
    List.reverse,
    -- ### Special folds
    Foldable.and, Foldable.or, Foldable.any, Foldable.all,
    Foldable.concat, Foldable.concatMap,
    -- ## Building lists
    -- ### Scans
    List.scanl, List.scanl1, List.scanr, List.scanr1,
    -- ### Infinite lists
    List.iterate, List.repeat, List.replicate, List.cycle,
    -- ## Sublists
    List.take, List.drop,
    List.takeWhile, List.dropWhile,
    List.span, List.break,
    List.splitAt,
    -- ## Searching lists
    Foldable.notElem,
    List.lookup,
    -- ## Zipping and unzipping lists
    List.zip, List.zip3,
    List.zipWith, List.zipWith3,
    List.unzip, List.unzip3,
    -- ## Functions on strings
    List.lines, List.words, List.unlines, List.unwords,

    -- # Converting to and from @String@
    -- ## Converting to @String@
    ShowS,
    Show(showsPrec, showList, show),
    shows,
    showChar, showString, showParen,
    -- ## Converting from @String@
    ReadS,
    Read(readsPrec, readList),
    reads, readParen, read, lex,

    -- # Basic Input and output
    IO,
    -- ## Simple I\/O operations
    -- All I/O functions defined here are character oriented.  The
    -- treatment of the newline character will vary on different systems.
    -- For example, two characters of input, return and linefeed, may
    -- read as a single newline character.  These functions cannot be
    -- used portably for binary I/O.
    -- ### Output functions
    putChar,
    putStr, putStrLn, print,
    -- ### Input functions
    getChar,
    getLine, getContents, interact,
    -- ### Files
    FilePath,
    readFile, writeFile, appendFile, readIO, readLn,
    -- ## Exception handling in the I\/O monad
    IOError, ioError, userError,

    -- # Generic Pretty-Printing
    Generic, Out

  ) where

import Control.Monad
import Control.Monad.Fail
import System.IO        hiding (print)
import System.IO.Error
import qualified Data.List as List
import Data.Either
import Data.Foldable    ( Foldable(..) )
import qualified Data.Foldable as Foldable
import Data.Functor     ( (<$>) )
import Data.Maybe
import Data.Traversable ( Traversable(..) )
import Data.Tuple
import Data.Semigroup

import GHC.Base hiding ( foldr, mapM, sequence )
import Text.Read
import GHC.Enum
import GHC.Num
import GHC.Real
import GHC.Float
import GHC.Show

import Text.PrettyPrint.GenericPretty
import GHC.Generics

-- ## Alternative definitions (INTEGER):
length :: Foldable t => t a -> Integer
length = fromIntegral . Foldable.length

-- ## Alternative definitions (PRINTING):
print           :: Out a => a -> IO ()
print x         =  pp x