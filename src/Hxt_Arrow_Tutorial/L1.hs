module Lessons.L1 where

import Text.XML.HXT.Core


play :: FilePath -> IO [XmlTree]
play = runX . processor

processor :: FilePath -> IOSArrow XmlTree XmlTree
processor filename =
    readDocument
    [withValidate no] -- "suppress validation (as my XML file cites no DTD) and leave other options as default"
    filename >>>
    putXmlTree -- pretty-print
    "-" -- stdout
