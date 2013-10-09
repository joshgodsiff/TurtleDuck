-- | A symbol table using a non empty list of maps
-- for use with the Turtle compiler for the second comp3610 assignment
-- by Robert 'Probie' Offner and Joshua Godsiff

module SymbolTable
    ( Identifier (..)
    , showIdentifier
    , SymbolTable
    , newSymbolTable
    , addSymbol
    , getSymbol
    , pushScope
    , popScope
    ) where

import Data.Map (Map)
import qualified Data.Map as M

-- Because I feel an urge to have one section of 'hip' Haskell
-- but don't want to rely on other libraries, I define comonads myself

class (Functor w) => Comonad w where
    coreturn :: w a -> a
    cojoin :: w a -> w (w a)

-- And now for the only comonad I use

data Cofree f a = a :< f (Cofree f a)

unwrap (_ :< xs) = xs

instance (Functor f) => Functor (Cofree f) where
    fmap f (x :< xs) = f x :< fmap (fmap f) xs
 
instance (Functor f) => Comonad (Cofree f) where
    coreturn (x :< _) = x
    cojoin x = x :< fmap cojoin (unwrap x)
    
-- | We differentiate identifiers by their names
-- and arity (so f, f(), f(x), f(x,y)) are all different functions
-- (except f which is a value)
data Identifier = Identifier { identName :: String
                             , arity :: Maybe Int
                             }
    deriving (Ord, Eq, Show)

showIdentifier :: Identifier -> String
showIdentifier (Identifier name arity) =
    maybe name (((name ++ "/") ++) . show) arity
    
-- | A symbol table is simply a non-empty list of scopes

type NonEmptyList = Cofree Maybe

type SymbolTable a = NonEmptyList (M.Map Identifier a)

addSymbol :: Identifier -> a -> SymbolTable a -> SymbolTable a
addSymbol key value (x :< xs) = M.insert key value x :< xs

getSymbol :: Identifier -> SymbolTable a -> Maybe a
getSymbol key (x :< xs) = maybe
    (maybe Nothing (getSymbol key) xs) -- Check next scope if possible
    Just -- Success
    (M.lookup key x) -- Is it in the current scope?

pushScope :: SymbolTable a -> SymbolTable a
pushScope scope = M.empty :< Just scope

popScope :: SymbolTable a -> SymbolTable a
popScope (_ :< (Just scope)) = scope
popScope _ = error "Popped final scope"

newSymbolTable :: SymbolTable a
newSymbolTable = M.empty :< Nothing
