module Tests.Bits.Optics (
    -- * Tests.
    tests,
) where

-- Imports.
-- Testing.
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)

-- Package.
-- Testing.
import Lib.Optics (test_isomorphism_laws)
import Lib.Gen (word8, tuple8Bool)

-- Module to test.
import Data.Bits.Optics (bitsLe, bitsBe)


{- | Test the @'bitsLe'@ isomorphism. -}
test_bitsLe :: TestTree
test_bitsLe =
    testProperty
        "Test bitsLe isomorphism"
        (test_isomorphism_laws ("bitsLe", bitsLe) word8 tuple8Bool)

{- | Test the @'bitsBe'@ isomorphism. -}
test_bitsBe :: TestTree
test_bitsBe =
    testProperty
        "Test bitsBe isomorphism"
        (test_isomorphism_laws ("bitsBe", bitsBe) word8 tuple8Bool)


{- | Test group for optics in the Bits module. -}
tests :: TestTree
tests =
    testGroup
        "Tests for the Data.Bits.Optics module."
        [test_bitsLe, test_bitsBe]
