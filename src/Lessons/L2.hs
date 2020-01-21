module Lessons.L2 where

import Text.XML.HXT.Core


r :: FilePath -> IOStateArrow s b XmlTree
r filename = readDocument [withValidate no] filename

-- runX $ r "data/a.xml" >>> title
title :: IOSArrow XmlTree String
title =
    getChildren >>>
    isElem >>> -- upstream of this we could receive a processing instruction, a comment, a segment of whitespace text, or an element. downstream, only elements.
    hasName "html" >>>
    getChildren >>>
    isElem >>> hasName "head" >>>
    getChildren >>>
    isElem >>> hasName "title" >>>
    getChildren >>>
    getText

-- runX $ r "data/a.xml" >>> elemPlus
elemPlus :: IOSArrow XmlTree XmlTree
elemPlus = getChildren

-- runX $ r "data/a.xml" >>> elem
elem :: IOSArrow XmlTree XmlTree
elem =
    getChildren >>>
    isElem

-- runX $ r "data/a.xml" >>> hasTitle
hasTitle :: IOSArrow XmlTree XmlTree
hasTitle =
    getChildren >>>
    isElem >>>
    hasName "html" >>>
    getChildren >>>
    isElem >>> hasName "head" >>>
    getChildren >>>
    isElem >>> hasName "title"
