module Lesson_1 where

import Text.XML.HXT.Core


myRead :: FilePath -> IO [XmlTree]
myRead arg = runX (processor arg)

processor :: FilePath -> IOSArrow XmlTree XmlTree
processor filename =
    readDocument
    [withValidate no] -- "suppress validation (as my XML file cites no DTD) and leave other options as default"
    filename >>>
    putXmlTree -- pretty-print
    "-" -- stdout
