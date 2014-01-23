module Graphics.QML.Test.GenURI where

import Test.QuickCheck.Gen
import Network.URI
import Numeric

capSize :: Int -> Gen a -> Gen a
capSize cap g = sized (\s -> if s > cap then resize cap g else resize s g)

uriGen :: Gen URI
uriGen = capSize 35 $ do
    let slists  = fmap (:[])
        listxyz = fmap concat . sequence
        listxs  = fmap concat . listOf
        listxs1 = fmap concat . listOf1
        lower   = elements $ slists $ enumFromTo 'a' 'z'
        upper   = elements $ slists $ enumFromTo 'A' 'Z'
        digit   = elements $ slists "01234567989"
        mark    = elements $ slists "-_.!~*'()"
        sextra  = elements $ slists "+-."
        rextra  = elements $ slists "$,;:@&=+"
        dash    = return "-"
        dot     = return "."
        alpha   = oneof [lower, upper]
        alphnum = oneof [lower, upper, digit]
        dchar   = oneof [lower, digit]
        dchar2  = frequency [(9,lower), (5,digit), (1,dash)]
        unres   = frequency [(9,alphnum), (1,mark)]
        scheme  = listxyz [lower, listxs $ frequency [
            (9,dchar), (1,sextra)]]
        dpart1  = listxyz [
                      frequency [(9,dchar), (1,listxyz [dchar, dot, dchar])],
                      listxs $ frequency [
                          (9,dchar2), (1,listxyz [dchar, dot, dchar]),
                          (1,listxyz [dchar, dot, dchar, dot, dchar])],
                      dchar, dot]
        dpart2  = oneof [lower, listxyz [lower, listxs dchar2, dchar]]
        regName = flip suchThat (\x -> length x < 255) $ listxyz [
            frequency [(9,dpart1), (1,return "")],
            dpart2, oneof [dot, return ""]]
        segment = fmap ('/':) $ listxs $ frequency [
            (9,unres), (1,rextra)]
        path   = listxs1 segment
    schemeStr <- scheme
    regNameStr <- regName
    pathStr <- path
    return $
        URI (schemeStr++":") (Just $ URIAuth "" regNameStr "") pathStr "" ""
