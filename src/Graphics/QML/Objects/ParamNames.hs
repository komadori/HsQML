{-# LANGUAGE
    ScopedTypeVariables,
    FlexibleInstances
  #-}

-- | Parameter Name Lists
module Graphics.QML.Objects.ParamNames (
    ParamNames,
    paramNames,
    noNames,
    fstName,
    plusName,
    anonParams,
    AnonParams ()
) where

-- | Represents a list of parameter names. The number of names in the list is
-- statically encoded using the length of the function type held in the type
-- parameter @a@.
newtype ParamNames a = ParamNames ([String] -> [String])

instance Show (ParamNames a) where
   show (ParamNames nsFunc) =
       let showHead [] = showString "noNames"
           showHead (n:ns) = showString "fstName " . shows n . showTail ns
           showTail [] = id
           showTail (n:ns) = showString " `plusName` " . shows n . showTail ns
       in showHead (nsFunc []) ""

-- | Coverts a 'ParamNames' list to an ordinary list of strings.
paramNames :: ParamNames a -> [String]
paramNames (ParamNames names) = names []

-- | Adds one parameter name to a 'ParamNames' list.
plusName :: ParamNames a -> String -> ParamNames (String -> a)
plusName (ParamNames ns) n = ParamNames $ ns . (n:)

-- | An empty 'ParamNames' list.
noNames :: ParamNames ()
noNames = ParamNames id

-- | Produces a 'ParamNames' list with a single name.
fstName :: String -> ParamNames (String -> ())
fstName = (noNames `plusName`)

-- | Helper class for generating anonymous parameter lists.
class AnonParams a where
    anonParams_ :: ParamNames a

instance (AnonParams b) => AnonParams (String -> b) where
    anonParams_ = plusName (anonParams_ :: ParamNames b) ""

instance AnonParams () where
    anonParams_ = noNames

-- | Polymorphically produces 'ParamNames' lists of any length filled with
-- blank parameter names.
anonParams :: (AnonParams a) => ParamNames a
anonParams = anonParams_
