module Lessons.L3 where

import Text.XML.HXT.Core
import Data.Char(toUpper)


-- runX $ r a >>> processor

a :: FilePath
a = "data/a.xml"

r :: FilePath -> IOSArrow XmlTree XmlTree
r f = readDocument [withValidate no] f

processor :: IOSArrow XmlTree (String,String)
processor =
    getChildren >>>
    isElem >>> hasName "html" >>>
    getChildren >>>
    isElem >>> hasName "head" >>>
    getChildren >>>
    isElem >>> hasName "meta" >>>
    getTuple

getTuple :: IOSArrow XmlTree (String,String)
getTuple =
    getAttrl -- each attribute (key-value association)
    >>> ( getName                     -- get the attr name
          &&&
          (getChildren >>> getText) ) -- get the attr value
    >>> ( arr (map toUpper)           -- change the name to upper-case
          ***
          returnA )                   -- don't change the value
