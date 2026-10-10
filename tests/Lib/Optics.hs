{-# LANGUAGE DataKinds #-}

module Lib.Optics (
    -- * Properties.
    -- ** Extensional equality of functions.
    test_extensional_equality,

    -- ** Optics laws.
    test_isomorphism_laws,
    test_prism_laws,
) where

-- Imports.
-- Base.
import Data.String (fromString)

-- Libraries.
import Optics.Core (Iso', view, review, Prism', preview)

-- Testing.
import Test.Falsify (Property, Gen, Predicate, (.$), assert, gen)
import Test.Falsify.Predicate (satisfies)


{- | Predicate for functional, extensional equality of functions. -}
isExtensionallyEqual
    :: Eq b
    => (String, a -> b)
    -> (String, a -> b)
    -> Predicate '[a]
isExtensionallyEqual (xs, f) (ys, g) =
    satisfies (fromString $ "Predicate " ++ xs ++ " == " ++ ys, \ x -> f x == g x)

{- | Test extensional equality of functions. -}
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
        forward = ("review " ++ xs ++ " . view " ++ xs, review i . view i)

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
        inverse = ("view " ++ xs ++ " . review " ++ xs, view i . review i)

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


{- | Test preview-review law of a prism. -}
test_prism_preview_review
    :: forall s a . (Eq a, Show a)
    => (String, Prism' s a)
    -> Gen a
    -> Property ()
test_prism_preview_review (xs, p) generator = do
        x <- gen generator
        assert $ isExtensionallyEqual forward ("Just", Just) .$ (fromString "x", x)
    where
        forward :: (String, a -> Maybe a)
        forward = ("preview " ++ xs ++ " . review " ++ xs, preview p . review p)

test_prism_review_preview
    :: forall s a . (Eq s, Show s)
    => (String, Prism' s a)
    -> Gen s
    -> Property ()
test_prism_review_preview (xs, p) generator = do
        s <- gen generator
        assert $ isExtensionallyEqual inverse endo .$ (fromString "s", s)
    where
        inverse :: (String, s -> Maybe s)
        inverse = ("fmap (review " ++ xs ++ ") . preview " ++ xs, fmap (review p) . preview p)

        endo :: (String, s -> Maybe s)
        endo = ("matchingElem " ++ xs, \ s -> maybe Nothing (const . Just $ s) (preview p s))

{- | Test isomorphism laws. -}
test_prism_laws
    :: forall s a . (Show s, Show a, Eq s, Eq a)
    => (String, Prism' s a)
    -> Gen s
    -> Gen a
    -> Property ()
test_prism_laws p genS genA
    =  test_prism_preview_review p genA
    *> test_prism_review_preview p genS
