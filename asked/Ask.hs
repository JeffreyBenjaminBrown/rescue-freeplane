XML, tree arrows, and Hxt: Transform text leaves into subtrees

# Background
The [Freeplane](https://www.freeplane.org/wiki/index.php/Home) app seems to have died its decades-long death. I'm extracting my data from it. Freeplane stores data as XML. One of the first steps I'm taking is to homogenize that format.

# Question, and what I wanted to do
My goal is to turn every `XText` item in an `.xml`-formatted file into an `XTag` called "node" with an attribute called "TEXT". I've got it done, but in a manner that seems inelegant.

I wanted to do this:

```haskell
do 
  t <- getText
  eelem "node" >>> addAttr "TEXT" t
```

But when I tried, I was informed that there is no monad instance for the [IOSLA arrow](https://hackage.haskell.org/package/hxt-9.3.1.18/docs/Control-Arrow-IOStateListArrow.html), hence `do`-notation is not available.

Is something like that possible?


# What I did instead

I dug into the raw, deeply recursive [XmlTree](https://hackage.haskell.org/package/hxt-9.3.1.18/docs/Text-XML-HXT-DOM-TypeDefs.html#t:XmlTree) data type, and ended up with:

```haskell
module Ask where

import Control.Category hiding ((.), id)
import Control.Arrow
import Text.XML.HXT.Core
import Data.Tree.NTree.TypeDefs


textToNode :: IOSArrow XmlTree XmlTree
textToNode = arr f where
  f :: XmlTree -> XmlTree
  f (NTree (XText s) children) =
    -- `children` is always an empty subtree for Text items
    let attrs = [ NTree
                  (XAttr $ mkName "TEXT")
                  [NTree (XText s) []] ]
    in NTree (XTag (mkName "node") attrs) children
  f x = x

go :: IO [XmlTree]
go =
  runX $
  readDocument [withValidate no] "flat.xml"
  >>> deepest (ifA isText textToNode none)
  >>> putXmlTree "-"
```

To see it in action, make a file called "flat.xml" containing:
```xml
<doc>
<a>1</a>
<b>2</b>
</doc>
```

When you run `go`, you'll get back the "1" and the "2", but inserted into `XTag`s like this:
```
---XTag "node"
   |   "TEXT"="1"
```

(You'll also get back some whitespace; it's not important.)
