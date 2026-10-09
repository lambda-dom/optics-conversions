module Tests.Bits.Optics (
    -- * Tests.
    tests,
) where

-- Imports.
-- Testing.
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.Falsify (testProperty)
import Test.Falsify.Generator (prim)

-- Package.
-- Testing.
import Lib.Optics (test_isomorphism_laws)
import Lib.Gen (word8, tuple8Bool, word16, tupleWord8, word32, tuple4Word8, tuple8Word8)

-- Module to test.
import Data.Bits.Optics (
    bitsLe,
    bitsBe,
    word16BytesLe,
    word16BytesBe,
    word32BytesLe,
    word32BytesBe,
    word64BytesLe,
    word64BytesBe)


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

{- | Test the @'word16BytesLe'@ isomorphism. -}
test_word16BytesLe :: TestTree
test_word16BytesLe =
        testProperty
            "test word16BytesLe isomorphism"
            (test_isomorphism_laws ("word16BytesLe", word16BytesLe) word16 tupleWord8)

{- | Test the @'word16BytesBe'@ isomorphism. -}
test_word16BytesBe :: TestTree
test_word16BytesBe =
        testProperty
            "test word16BytesBe isomorphism"
            (test_isomorphism_laws ("word16BytesBe", word16BytesBe) word16 tupleWord8)

{- | Test the @'word32BytesLe'@ isomorphism. -}
test_word32BytesLe :: TestTree
test_word32BytesLe =
        testProperty
            "test word32BytesLe isomorphism"
            (test_isomorphism_laws ("word32BytesLe", word32BytesLe) word32 tuple4Word8)

{- | Test the @'word32BytesBe'@ isomorphism. -}
test_word32BytesBe :: TestTree
test_word32BytesBe =
        testProperty
            "test word32BytesBe isomorphism"
            (test_isomorphism_laws ("word32BytesBe", word32BytesBe) word32 tuple4Word8)

{- | Test the @'word64BytesLe'@ isomorphism. -}
test_word64BytesLe :: TestTree
test_word64BytesLe =
        testProperty
            "test word64BytesLe isomorphism"
            (test_isomorphism_laws ("word64BytesLe", word64BytesLe) prim tuple8Word8)

{- | Test the @'word64BytesBe'@ isomorphism. -}
test_word64BytesBe :: TestTree
test_word64BytesBe =
        testProperty
            "test word64BytesBe isomorphism"
            (test_isomorphism_laws ("word64BytesBe", word64BytesBe) prim tuple8Word8)


{- | Test group for optics in the Bits module. -}
tests :: TestTree
tests =
    testGroup
        "Tests for the Data.Bits.Optics module."
        [
            test_bitsLe,
            test_bitsBe,
            test_word16BytesLe,
            test_word16BytesBe,
            test_word32BytesLe,
            test_word32BytesBe,
            test_word64BytesLe,
            test_word64BytesBe
        ]
