module Main (main) where

import Data.Binary as B
import Test.Tasty
import Test.Tasty.QuickCheck as QC

import Control.Monad.Caching.ResponseMeta

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

decodeEncodeBinary :: ResponseMeta -> Property
decodeEncodeBinary sres = sres === (B.decode . B.encode) sres

qcProps :: TestTree
qcProps = testGroup "By Quickcheck"
  [ QC.testProperty "Binary: decode . encode . (:: ResponseMeta) = id" decodeEncodeBinary
  ]
