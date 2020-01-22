module Mine where

import Control.Category hiding ((.), id)
import Control.Arrow
import Control.Arrow.ArrowList
import Control.Arrow.ListArrow
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree
import Text.XML.HXT.Core


go :: FilePath -> IOSLA (XIOState ()) XmlTree c -> IO [c]
go file func =
  runX $
  readDocument [withValidate no] file >>>
  func

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
