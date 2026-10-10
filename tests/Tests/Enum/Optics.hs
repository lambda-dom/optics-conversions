module Tests.Enum.Optics (
    -- * Tests.
    tests,
) where

-- Imports.
-- Libraries.
import Optics.Core (Prism')

-- Testing.
import Test.Tasty (TestTree, testGroup)
import Test.Falsify (Gen)
import Test.Tasty.Falsify (testProperty)
import Test.Falsify.Generator (bool, int)
import Test.Falsify.Range (uniform)

-- Package.
import Lib.Optics (test_prism_laws)
import Lib.Gen (enumFull)

-- Module to test.
import Data.Enum.Optics (enum)


{- | Test the @'enum'@ prism on 'Bool'. -}
test_enum_bool :: TestTree
test_enum_bool =
        testProperty
            "test enum prism on Bool enumeration"
            (test_prism_laws ("enum", p) (int uniform) (bool False))
    where
        p :: Prism' Int Bool
        p = enum

{- | Test the @'enum'@ prism on 'Ordering'. -}
test_enum_ordering :: TestTree
test_enum_ordering =
        testProperty
            "test enum prism on Ordering enumeration"
            (test_prism_laws ("enum", p) (int uniform) g)
    where
        p :: Prism' Int Ordering
        p = enum

        g :: Gen Ordering
        g = enumFull


{- | Test group for optics in the Bits module. -}
tests :: TestTree
tests =
    testGroup
        "Tests for the Data.Enum.Optics module."
        [test_enum_bool, test_enum_ordering]
