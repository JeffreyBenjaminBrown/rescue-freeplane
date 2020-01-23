module Research where

import Data.Text (strip, pack, unpack)
import qualified Data.Set as S

import Control.Category hiding ((.), id)
import Control.Arrow
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree

import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs


go :: FilePath -> IOSLA (XIOState ()) XmlTree c -> IO [c]
go file func =
  runX $
  readDocument [withValidate no] file >>>
  func

uniq :: Ord a => [a] -> [a]
uniq = S.toList . S.fromList

-- uniq <$> go "data/test/big.mm" allTags
allTags :: IOSArrow XmlTree String
allTags = multi getName

-- | TODO ? I thought I'd print leaves differently from non-leaf subtrees --
-- including leading asterisks except for leaves.
-- But then I realized that a leaf and a subtree could be neighbors.
-- If the leaf came second, it would be swallowed by the subtree.
-- go "data/test/node-and-leaf.mm" $ printOrg 0
printOrg :: Int -> IOSArrow XmlTree XmlTree
printOrg i0 = let
  printElem :: Int -> IOSArrow XmlTree XmlTree
  printElem i =
    ifA (hasName "leaf") -- the other alternative is "node"
    ( perform $ getAttrValue "TEXT" >>> arrIO putStrLn)
    ( perform $ getAttrValue "TEXT" >>> arrIO
      (\s -> putStrLn $ (replicate i '*') ++ " " ++ s) )
  printIfApplicable :: Int -> IOSArrow XmlTree XmlTree
  printIfApplicable i = ifA isElem (printElem i) returnA
  in printIfApplicable i0 >>> getChildren >>>
     printOrg (i0 + 1)

-- | Print every node, top-down, depth-first.
-- go "data/test/leaves-have-tags.xml" printEverything
printEverything :: IOSArrow XmlTree XmlTree
printEverything =
  processTopDown $ perform
  $ ( ifA (hasAttr "DEPTH") (getAttrValue "DEPTH")
      -- if it has a depth, show that
      (ifA isElem getName (arr $ const "blank") ) )
      -- if it has a name, show that; otherwise, show "blank"
  >>> arrIO putStrLn

-- go "data/test/leaves-have-tags.xml" getDepthAttr
getDepthAttr :: IOSArrow XmlTree String
getDepthAttr =
  deepest (isElem >>> getAttrValue "DEPTH")

-- go "data/test/leaves-have-tags.xml" tagLeaves
tagLeaves :: IOSArrow XmlTree XmlTree
tagLeaves =
  delete_ifBlank_bottomUp
  >>> processTopDown ( ifA (isElem >>> getChildren) returnA
                       $ changeQName (const $ mkName "leaf") )
  >>> putXmlTree "-"

-- go "data/test/rich-content-parents.xml" $ deep promoteRichContent
promoteRichContent :: IOSArrow XmlTree XmlTree
promoteRichContent =
  processTopDown (ifA isRichContentParent getChildren returnA)
  >>> putXmlTree "-"

-- go "data/test/rich-content-parents.xml" $ deep isRichContentParent
isRichContentParent :: IOSArrow XmlTree String
isRichContentParent = let
  f = ( hasName "node" &&&
        (getChildren >>> hasName "richcontent") )
      >>> arrL ( \(h,r) ->
                   if not (null h) && not (null r)
                   then [True]
                   else [] )
  in ifA f (getAttrValue "A") none

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

-- go "data/test/flat.xml" deleteIf_multiplConditions
deleteIf_multiplConditions :: IOSArrow XmlTree XmlTree
deleteIf_multiplConditions =
  ( processTopDown $
    ifA (hasName "b" <+> isText) none returnA )
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
