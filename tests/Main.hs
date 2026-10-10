-- Imports.
-- Testing.
import Test.Tasty (defaultMain, testGroup)

-- Package.
import Tests.Bits.Optics qualified as Bits (tests)
import Tests.Enum.Optics qualified as Enum (tests)


-- Main test driver.
main :: IO ()
main =
    defaultMain $
        testGroup
        "Package tests."
        [Bits.tests, Enum.tests]
