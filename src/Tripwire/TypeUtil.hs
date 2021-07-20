module Tripwire.TypeUtil where

import Relude

type AllC :: (k -> Constraint) -> [k] -> Constraint
type family AllC c xs where
    AllC c '[]      = ()
    AllC c (x : xs) = (c x, AllC c xs)
    
type FMap :: (a -> b) -> [a] -> [b]
type family FMap f xs where
    FMap f '[]      = '[]
    FMap f (x : xs) = f x : FMap f xs
    
type RemoveID :: (Type -> Type) -> Type -> Type
type family RemoveID f x where
    RemoveID Identity x = x
    RemoveID f x = f x
    