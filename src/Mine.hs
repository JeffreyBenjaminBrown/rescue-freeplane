module Mine where

import Data.Text hiding (null)

import Control.Category hiding ((.), id)
import Control.Arrow
import Control.Arrow.ArrowList
import Control.Arrow.ListArrow
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree

import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs


go :: FilePath -> IOSLA (XIOState ()) XmlTree c -> IO [c]
go file func =
  runX $
  readDocument [withValidate no] file >>>
  func

-- | TODO ? This would be easier, but do notation is unavailable.
-- do
--   t <- getText
--   eelem "node" >>> addAttr "TEXT" t
-- to execute, use test_textToNode, below
textToNode :: IOSArrow XmlTree XmlTree
textToNode = arr f where
  f :: XmlTree -> XmlTree
  f (NTree (XText s) children) =
    let attrs = [ NTree
                  (XAttr $ mkName "TEXT")
                  [NTree (XText s) []]
                , NTree (XAttr $ mkName "leaf") [] ]
    in NTree (XTag (mkName "node") attrs) children
  f x = x

test_textToNode :: IO [XmlTree]
test_textToNode =
  go "data/test/tiny-flat.xml"
  $ deepest (ifA isText textToNode none)
  >>> putXmlTree "-"

-- go "data/test/scattered-across-levels.xml" replaceWithChildren_ifX
replaceWithChildren_ifX :: IOSArrow XmlTree XmlTree
replaceWithChildren_ifX =
  processTopDown (ifA (isElem >>> hasName "x") getChildren returnA)
  >>> putXmlTree "-"

-- go "data/test/flat.xml" delete_ifBlank_bottomUp
delete_ifBlank_bottomUp :: IOSArrow XmlTree XmlTree
delete_ifBlank_bottomUp = let
  isToSkip :: IOSArrow XmlTree String
  isToSkip = isText >>> getText >>> isA (null . strip')
  in processBottomUp (ifA isToSkip none returnA)
     >>> putXmlTree "-"

-- go "data/test/flat.xml" deleteIf
deleteIf :: IOSArrow XmlTree XmlTree
deleteIf =
  getChildren >>> getChildren
  >>> ifA (isElem >>> hasName "b") none returnA
  >>> putXmlTree "-"

-- go "data/test/scattered-across-levels.xml" getDeepest_xTags
getDeepest_xTags :: IOSArrow XmlTree XmlTree
getDeepest_xTags =
  deepest (isElem >>> hasName "x")
  >>> putXmlTree "-"

-- go "data/test/scattered-across-levels.xml" getHighest_xTags
getHighest_xTags :: IOSArrow XmlTree XmlTree
getHighest_xTags =
  deep (isElem >>> hasName "x")
  >>> putXmlTree "-"

strip' :: String -> String
strip' = unpack . strip . pack
