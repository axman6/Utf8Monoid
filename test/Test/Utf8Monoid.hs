module Test.Utf8Monoid (module Test.Utf8Monoid) where

import Data.Proxy (Proxy(..))
import Test.Tasty (TestTree)
import Test.Tasty.QuickCheck.Laws.Monoid

import Utf8Monoid

test_monoid_laws :: TestTree
test_monoid_laws = testMonoidLaws (Proxy :: Proxy Utf8Monoid)