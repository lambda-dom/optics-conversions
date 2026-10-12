{- |
Module: Lib.Names

Utilities for function name generation in property testing.
-}

module Lib.Names (
    -- * Predicates.
    isIdentifier,
    isOperator,

    -- * Category of named functions.
    Fn,

    -- ** Constructors.
    makeFn,
    makeOp,

    -- ** Some specific values.
    diagonal,
    proj1,
    proj2,
) where

-- Imports.
-- Base.
import Control.Category (Category)
import qualified Control.Category as Category (id, (.))
import Data.Char (isAscii, isLetter, isAlphaNum)


{- | Surround a string with parens. -}
surroundParens :: String -> String
surroundParens xs = '(' : (xs ++ [')'])

{- | 'True' is string is a valid identifier. -}
isIdentifier :: String -> Bool
isIdentifier []       = False
isIdentifier (c : cs) = (isAscii c && isLetter c) && isRest cs
    where
        isRest []       = True
        isRest (d : ds) = isAscii d && (isAlphaNum d || '_' == d || '-' == d) && isRest ds

{- | 'True' is string is a valid operator. -}
isOperator :: String -> Bool
isOperator [] = False
isOperator xs = all (`elem` "<>|$%/=?+*.") xs


{- | A function annotated with a name. -}
data Fn a b = Fn {
    name     :: String,
    function :: a -> b
}

-- Instances.
instance Show (Fn a b) where
    show :: Fn a b -> String
    show = name

instance Category Fn where
    id :: Fn a a
    id = Fn "id" Prelude.id

    (.) :: Fn b c -> Fn a b -> Fn a c
    (.) fn1 fn2 = Fn {
            name     = name fn1 ++ " . " ++ name fn2,
            function = (function fn1 . function fn2)
        }

{- | Construct a named function from an infix operator. -}
makeFn :: String -> (a -> b) -> Maybe (Fn a b)
makeFn xs f = if isIdentifier xs then Just $ Fn {name = xs, function = f} else Nothing

{- | Construct a named function from an infix operator. -}
makeOp :: String -> (a -> a -> b) -> Maybe (Fn (a, a) b)
makeOp xs op = if isOperator xs then Just $ Fn {name = ys, function = uncurry op} else Nothing
    where
        ys = "uncurry" ++ surroundParens xs


{- | The diagonal function. -}
diagonal :: Fn a (a, a)
diagonal = Fn {name = "diagonal", function = (\ x -> (x, x))}

{- | The first projection. -}
proj1 :: Fn (a, b) a
proj1 = Fn {name = "fst", function = fst}

{- | The first projection. -}
proj2 :: Fn (a, b) b
proj2 = Fn {name = "snd", function = snd}
