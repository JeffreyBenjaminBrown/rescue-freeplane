-- https://www.vex.net/~trebla/haskell/hxt-arrow/lesson-0.xhtml

module Lesson_0 where

import Text.XML.HXT.Core


play :: Int -> IO [Int]
play = runX -- run the arrow on a degenerate one-node XML document
       . dumb

dumb :: Int
     -> IOSArrow a Int -- an HXT arrow that ignores its input and produces [Ints] as output. (The output is always a list, so the brackets are omitted from the type signature.)
dumb n =
    arr (const n) <+> -- IOSArrow XmlTree Int
    arr (const (n+1)) >>> -- IOSArrow XmlTree Int
    returnA <+> arr (* 2) >>> -- IOSArrow Int Int
    isA (<= 60) -- IOSArrow Int Int, a filter
