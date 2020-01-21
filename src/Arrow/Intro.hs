-- based on the Hackage docs for the imported libraries

{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Arrow.Intro where

import Control.Category hiding ((.), id)
import Control.Arrow


-- | Control.Category only defines two new operators, >>> and <<<.
-- And aside from some Kleisli thing, the only nontrivial Category
-- instance defined in Control.Category is for (->).
demo_category :: Bool
demo_category = and
  [ "43" == (show >>> reverse) 34
  , "43" == (reverse <<< show) 34
  , "43" == (reverse .   show) 34 ]

demo_first :: Bool
demo_first = first (+1) (1,2) == (2,2)

demo_second :: Bool
demo_second = second (+1) (1,2) == (1,3)

-- | *** = "treat separately"
demo_star_cubed :: Bool
demo_star_cubed = ((+1) *** (+2)) (0,0)
                  == (1,2)

-- | returnA and arr are both like id, in different ways.
demo_degenerate :: Bool
demo_degenerate = and
   [ (    id      ***     (+2)) (0,0) == (0,2)
   , (    returnA ***     (+2)) (0,0) == (0,2)
   , (arr id      ***     (+2)) (0,0) == (0,2)
   , (arr returnA ***     (+2)) (0,0) == (0,2)
   , (    id      *** arr (+2)) (0,0) == (0,2)
   , (    returnA *** arr (+2)) (0,0) == (0,2) ]

demo_fanout :: Bool
demo_fanout =
  ((+1) &&& (+2)) 0
  == (1,2)

-- | left = do to left and leave right unchanged,
-- and vice-versa.
demo_left_and_right :: Bool
demo_left_and_right = and
  [ left  (+1) (Left  0) == (Left  1 :: Either Int Int)
  , left  (+1) (Right 0) == (Right 0 :: Either Int Int)
  , right (+1) (Right 0) == (Right 1 :: Either Int Int)
  , right (+1) (Left  0) == (Left  0 :: Either Int Int) ]

-- | +++ = cover both cases
demo_plus_cubed :: Bool
demo_plus_cubed = and
  [ ((+1) +++ (+2)) (Left  0) == (Left  1 :: Either Int Int)
  , ((+1) +++ (+2)) (Right 0) == (Right 2 :: Either Int Int) ]

demo_fanin :: Bool
demo_fanin = and
  [ ((+1) ||| (+2)) (Left  0) == 1
  , ((+1) ||| (+2)) (Right 0) == 2 ]
