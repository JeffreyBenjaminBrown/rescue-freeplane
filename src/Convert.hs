module Convert where

import Data.Text (strip, pack, unpack)

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

together =
  go "data/test/big.mm" $
  delete_ifBlank_bottomUp >>>
  nodesOnly >>>
  putXmlTree "-"

nodesOnly :: IOSArrow XmlTree XmlTree
nodesOnly = getChildren >>> getChildren >>> isElem >>> hasName "node"

delete_ifBlank_bottomUp :: IOSArrow XmlTree XmlTree
delete_ifBlank_bottomUp = let
  isToSkip :: IOSArrow XmlTree String
  isToSkip = isText >>> getText >>> isA (null . unpack . strip . pack)
  in processBottomUp (ifA isToSkip none returnA)
