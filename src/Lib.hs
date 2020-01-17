module Lib where

import Text.XML.HXT.Core


myRead :: FilePath -> IO [XmlTree]
myRead arg = runX (processor arg)

processor :: FilePath -> IOSArrow XmlTree XmlTree
processor filename =
    readDocument [withValidate no] filename >>>
    putXmlTree "-"
