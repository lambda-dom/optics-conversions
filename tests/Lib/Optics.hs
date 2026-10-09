{-# LANGUAGE DataKinds #-}

module Lib.Optics (
    -- * Properties.
    test_extensional_equality,
    test_isomorphism_laws,
) where

-- Imports.
-- Base.
import Data.String (fromString)

-- Libraries.
import Optics.Core (Iso', view, review)

-- Testing.
import Test.Falsify (Property, Gen, Predicate, (.$), assert, gen)
import Test.Falsify.Predicate (prettyExpr, unary)


{- | Predicate for functional, extensional equality. -}
isExtensionallyEqual
    :: Eq b
    => (String, a -> b)
    -> (String, a -> b)
    -> Predicate '[a]
isExtensionallyEqual (xs, f) (ys, g) =
    unary
        (\ x -> f x == g x)
        (\ e -> let zs = " applied to " ++ prettyExpr e in xs ++ zs ++ " not equal to " ++ ys ++ zs)


{- | Functional equality predicate. -}
test_extensional_equality
    :: (Show a, Eq b)
    => (String, a -> b)
    -> (String, a -> b)
    -> Gen a
    -> Property ()
test_extensional_equality p q genArg = do
        x <- gen genArg
        assert $ isExtensionallyEqual p q .$ (fromString "x", x)

{- | Test the forward law of an @Iso\'@. -}
test_isomorphism_laws_forward
    :: forall a b . (Show a, Eq a)
    => (String, Iso' a b)
    -> Gen a
    -> Property ()
test_isomorphism_laws_forward (xs, i) generator = do
        x <- gen generator
        assert $ isExtensionallyEqual forward ("identity", id) .$ (fromString "x", x)
    where
        forward :: (String, a -> a)
        forward = ("Forward direction of " ++ xs, review i . view i)

{- | Test the inverse law of an @Iso\'@. -}
test_isomorphism_laws_inverse
    :: forall a b . (Show b, Eq b)
    => (String, Iso' a b)
    -> Gen b
    -> Property ()
test_isomorphism_laws_inverse (xs, i) generator = do
        y <- gen generator
        assert $ isExtensionallyEqual inverse ("identity", id) .$ (fromString "y", y)
    where
        inverse :: (String, b -> b)
        inverse = ("Inverse direction of " ++ xs, view i . review i)


{- | Test isomorphism laws. -}
test_isomorphism_laws
    :: forall a b . (Show a, Show b, Eq a, Eq b)
    => (String, Iso' a b)
    -> Gen a
    -> Gen b
    -> Property ()
test_isomorphism_laws p genA genB
    =  test_isomorphism_laws_forward p genA
    *> test_isomorphism_laws_inverse p genB
