-- | Most (all?) of the utilities here are demonstrated in Research.hs.
--
-- Here's what this does to the various XML tags in an .mm file:
--   print: node
--   maybe someday: arrowlink, font
--   promote children: map, body, html, p, richcontent
--   skip: edge, head, hook, properties
-- (find all the tags via Research.allTags)

{-# LANGUAGE LambdaCase,
ScopedTypeVariables,
Arrows #-}

module Convert where

import System.IO
import Data.Text (strip, pack, unpack)

import Control.Category hiding ((.), id)
import Control.Arrow
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree

import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs


convert :: FilePath -> IO ()
convert n = convert' n n

-- | Strip the file extensions from the paths used as input.
-- They are fixed to .mm and .org automatically,
-- to avoid accidentally overwriting the input with the output.
convert' :: FilePath -> FilePath -> IO ()
convert' iName oName = do
  h <- openFile (oName ++ ".org") ReadWriteMode
  _ <-
    runX $
    readDocument [withValidate no] (iName ++ ".mm") >>>
    fromLA ( processBottomUp delete_skippable >>>
             processTopDown promoteRichContent >>>
               -- PITFALL: promote rich content tags first,
               -- because `promoteOtherChildren` removes them.
             processTopDown promoteOtherChildren >>>
             processBottomUp textToNode >>>
             processTopDown noBullets >>>
             getChildren ) >>> -- to skip the "/" node
    printOrg h 1 -- or putXmlTree "-"
  hClose h

printOrg :: Handle -> Int -> IOSArrow XmlTree XmlTree
printOrg h i0 = let
  printBulleted :: Int -> String -> IO ()
  printBulleted i s =
    hPutStrLn h $ replicate i '*' ++ " " ++ strip' s
  printNoBullets :: String -> IO ()
  printNoBullets s =
    hPutStrLn h $                           strip' s
  printElem :: Int -> IOSArrow XmlTree XmlTree
  printElem i = ifA isElem
                ( ifA (hasName "node")
                  ( perform $ getAttrValue "TEXT" >>>
                    arrIO (printBulleted i) )
                  ( ifA (hasName "noBullets")
                    ( perform $ getAttrValue "TEXT" >>>
                      arrIO printNoBullets )
                    returnA ) )
                returnA
  in if i0 < 1
     then error "printOrg: should have a positive number of bullets"
     else printElem i0 >>> getChildren >>>
          printOrg h (i0 + 1)

-- | If a node has exactly one child and no grandchildren,
-- the child should be shown without leading org-mode bullets.
-- This changes its tag to "noBullets",
-- so that the print function knows how to treat it.
noBullets :: LA XmlTree XmlTree
noBullets = let
  zeroGrandchildren :: LA XmlTree Bool
  zeroGrandchildren = (getChildren >>> getChildren) >>. zero
    where zero = \case [ ] -> [True]; _ -> []
  oneChild :: LA XmlTree XmlTree
  oneChild = proc x ->
    -- the \case statement here is like `zero` above,
    -- but it returns the input XmlTree rather than a Bool.
    -- If it returned a Bool, I could not plug it into
    -- `zeroGrandchildren` in the condition below.
    getChildren >>. (\case [_] -> [x]; _ -> []) -<< x
  cn :: LA XmlTree XmlTree
  cn = changeQName $ const $ mkName "noBullets"
  cnMap :: [XmlTree] -> [XmlTree]
  cnMap = map (head . runLA cn)
  in ifA ( isElem >>> oneChild >>> zeroGrandchildren )
      (changeChildren cnMap)
      returnA

textToNode :: LA XmlTree XmlTree
textToNode = let
  f :: XmlTree -> XmlTree
  f (NTree (XText s) children) =
    let attrs = [ NTree
                  (XAttr $ mkName "TEXT")
                  [NTree (XText s) []] ]
    in NTree (XTag (mkName "node") attrs) children
  f x = x
  in arr f

promoteOtherChildren :: LA XmlTree XmlTree
promoteOtherChildren = let
  tagsToVanish = ["map","body","html","richcontent","p"]
  in ifA ( foldr1 (<+>) $
           map hasName tagsToVanish)
     getChildren returnA

-- | This relies on a certain .mm format property:
-- a <node> with a <richcontent> child has no other children,
-- and no text.
promoteRichContent :: LA XmlTree XmlTree
promoteRichContent = let
  isRichContentParent :: LA XmlTree Bool
  isRichContentParent =
    ( hasName "node" &&&
      (getChildren >>> hasName "richcontent") )
    >>> arrL ( \(h,r) -> if not (null h) && not (null r)
                         then [True]
                         else [] )

  in ifA isRichContentParent getChildren returnA

-- | Deletes whole subtrees.
delete_skippable :: LA XmlTree XmlTree
delete_skippable = let
  -- isBlankText, isTagToSkip :: IOSArrow XmlTree XmlTree
  isBlankText = isText >>>
    ifA (getText >>> isA (null . strip'))
    returnA none
  isTagToSkip = isElem >>>
    ( hasName "edge" <+>
      hasName "head" <+>
      hasName "hook" <+>
      hasName "arrowlink" <+> -- TODO ? someday
      hasName "font" )        -- TODO ? someday
  in ifA (isBlankText <+> isTagToSkip) none returnA

strip' :: String -> String
strip' = unpack . strip . pack
