module Mine where

import Control.Category hiding ((.), id)
import Control.Arrow
import Control.Arrow.ArrowList
import Control.Arrow.ListArrow
import Control.Arrow.ArrowIf
import Control.Arrow.ArrowTree
import Text.XML.HXT.Core


-- To run any `arrow` below, use e.g.
-- runX $ r "data/test/x.xml" >>> arrow

r :: FilePath -> IOStateArrow s b XmlTree
r filename = readDocument [withValidate no] filename

getDeepest_xTags :: IOSArrow XmlTree XmlTree
getDeepest_xTags =
  deepest (isElem >>> hasName "x")
  >>> putXmlTree "-"

getHighest_xTags :: IOSArrow XmlTree XmlTree
getHighest_xTags =
  deep (isElem >>> hasName "x")
  >>> putXmlTree "-"
