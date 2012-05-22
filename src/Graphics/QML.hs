module Graphics.QML (
-- * Overview
-- * Script-side APIs
{-|
/Properties/

[@title : string@] Window title.

[@visible : bool@] Window visibility.

/Methods/

[@open(url)@] Opens a child window displaying a QML document loaded from the
supplied @url@. Returns the child's window object.

[@close()@] Closes all children, then closes any window associated with
this context, and finally destroys the context.

[@sendToParent(obj)@]

[@sendToChild(wnd, obj)@]

/Signals/

[@onParent(obj)@]

[@onChild(wnd, obj)@]

 -}
-- * Graphics.QML
{-|
This module imports the entire package excepting 'Graphics.QML.Debug'.
 -}
  module Graphics.QML.Engine,
  module Graphics.QML.Marshal,
  module Graphics.QML.Objects
) where

import Graphics.QML.Engine
import Graphics.QML.Marshal
import Graphics.QML.Objects
