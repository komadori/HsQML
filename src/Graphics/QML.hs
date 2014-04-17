-- | This module imports the entire package, except 'Graphics.QML.Debug'.
module Graphics.QML (
-- * Overview
{-|
HsQML layers a low to medium level Haskell API on top of the C++ Qt Quick
framework. It allows you to write graphical applications where the front-end is 
written in Qt Quick's QML language (incorporating JavaScript) and the back-end
is written in Haskell. To this end, this library provides two pieces of
functionality:-

The 'Graphics.QML.Engine' module allows you to create windows which host QML
content. You can specify a custom global object to be made available to the
JavaScript running inside the content. In this way, the content can interface
with the Haskell program.

The 'Graphics.QML.Objects' module allows you to define your own custom object
types which can be marshalled between Haskell and JavaScript.
 -}
-- * Graphics.QML
  module Graphics.QML.Engine,
  module Graphics.QML.Marshal,
  module Graphics.QML.Objects
) where

import Graphics.QML.Engine
import Graphics.QML.Marshal
import Graphics.QML.Objects
