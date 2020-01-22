module Convert where

import Data.Text (strip, pack, unpack)

import Control.Category hiding ((.), id)
import Control.Arrow
import Control.Arrow.ArrowList
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree

import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs


new =
  go "data/test/big.mm" $
  delete_skippable >>>
  promoteChildren >>>
  putXmlTree "-"

old =
  go "data/test/big.mm" $
  delete_skippable >>>
  putXmlTree "-"

go :: FilePath -> IOSLA (XIOState ()) XmlTree c -> IO [c]
go file func =
  runX $
  readDocument [withValidate no] file >>>
  func

promoteChildren :: IOSArrow XmlTree XmlTree
promoteChildren = let
  tagsToVanish = ["map","body","html","richcontent","p"]
  in processBottomUp $
  ifA (foldr1 (<+>) $ map hasName tagsToVanish) getChildren returnA

delete_skippable :: IOSArrow XmlTree XmlTree
delete_skippable = let
  isBlankText :: IOSArrow XmlTree XmlTree
  isBlankText = isText >>>
    ifA (getText >>> isA (null . unpack . strip . pack))
    returnA none
  isTagToSkip :: IOSArrow XmlTree XmlTree
  isTagToSkip = isElem >>>
    ( hasName "edge" <+>
      hasName "head" <+>
      hasName "hook" <+>
      hasName "arrowlink" <+> -- TODO ? someday
      hasName "font" )        -- TODO ? someday
  in processBottomUp (ifA (isBlankText <+> isTagToSkip) none returnA)
