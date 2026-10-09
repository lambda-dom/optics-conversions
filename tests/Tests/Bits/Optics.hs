module Tests.Bits.Optics (
    -- * Tests.
    test_bitsLe,
    test_bitsBe,
) where

-- Imports.
-- Testing.
import Test.Falsify (Property)

-- Package.
-- Testing.
import Lib.Optics (test_isomorphism_laws)
import Lib.Gen (word8, tuple8Bool)

-- Module to test.
import Data.Bits.Optics (bitsLe, bitsBe)


{- | Test the @'bitsLe'@ isomorphism. -}
test_bitsLe :: Property ()
test_bitsLe = test_isomorphism_laws ("bitsLe", bitsLe) word8 tuple8Bool

{- | Test the @'bitsBe'@ isomorphism. -}
test_bitsBe :: Property ()
test_bitsBe = test_isomorphism_laws ("bitsBe", bitsBe) word8 tuple8Bool
