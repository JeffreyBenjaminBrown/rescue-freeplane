module L2 where

import Text.XML.HXT.Core


play arg = do
  results <- runX (processor arg)
  print results

processor :: FilePath -> IOSArrow XmlTree String
processor filename =
    readDocument [withValidate no] filename >>>
    getChildren >>>
    isElem >>> hasName "html" >>>
    getChildren >>>
    isElem >>> hasName "head" >>>
    getChildren >>>
    isElem >>> hasName "title" >>>
    getChildren >>>
    getText
