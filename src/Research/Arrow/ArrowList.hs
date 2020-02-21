-- based on the Hackage docs for the imported libraries

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Research.Arrow.ArrowList where

import Control.Category hiding ((.), id)
import Control.Arrow.ArrowList
import Control.Arrow.ListArrow
  -- defines newtype LA = LA {runLA :: a -> [b]}


-- | `arr2 :: ArrowList a => (b1 -> b2 -> c) -> a (b1, b2) c`
-- For the `LA` arrow, `arr2` turns a 2-argument function
-- into a function from pairs to singleton lists.
demo_arr2 :: Bool
demo_arr2 = let
  la :: LA (Int,Int) Int
  la = arr2 $ \a b -> a+b
  in runLA la (1,2) == [3]

-- obvious:
-- arr3 :: (b1 -> b2 -> b3 -> c) -> a (b1, (b2, b3)) c
-- arr4 :: (b1 -> b2 -> b3 -> b4 -> c) -> a (b1, (b2, (b3, b4))) c

-- | `arr2A :: (b -> a c d) -> a (b, c) d`
-- For the `LA` arrow, `arr2A` takes a function that returns an `LA`,
-- and returns an `LA` of two arguments.
demo_arr2A :: Bool
demo_arr2A = let
  la :: LA (Int,Int) Int
  la = arr2A (\x -> LA $ \y -> [y,x+y])
  in runLA la (1,10) == [10,11]

-- | `arrL :: (b -> [c]) -> a b c`
-- `arrL` seems to be the most natural way to create an `LA`.
demo_arrL :: Bool
demo_arrL = let
  f x = [0,x,x]
  in runLA (arrL f) 3 == [0,3,3]

-- obvious:
-- arr2L :: (b -> c -> [d]) -> a (b, c) d

-- | `constA :: c   -> a b c`
--   `constL :: [c] -> a b c`
-- These are natural entry points for the `LA` type.
demo_constA :: Bool
demo_constA = let
  f :: LA () Int
  f = constA 0 >>> arrL (\x -> [x+1,x+2])
  in runLA f () == [1,2]
demo_constL :: Bool
demo_constL = let
  f :: LA () Int
  f = constL [1,6,11] >>> isA (>5)
  in runLA f () == [6,11]

-- | `(>>.) :: a b c -> ([c] -> [d]) -> a b d`
-- I would call this "transform the list into another list".
demo_gtgtdot :: Bool
demo_gtgtdot = let
  f x = [x,x**2]
  in runLA (arrL f >>. reverse) 0.5 == [0.25,0.5]

-- | `(>.) :: a b c -> ([c] -> d) -> a b d`
-- I would call this "transform the list into a singleton".
demo_gtdot :: Bool
demo_gtdot = let
  f x = [x,x**2]
  in runLA (arrL f >. maximum) (0.5 :: Float) == [0.5]

demo_isA :: Bool
demo_isA = let
  f = runLA (isA (> 5))
  in and [ f 4 == []
         , f 6 == [6] ]
