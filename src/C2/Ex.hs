module C2.Ex where


import Data.Kind

-- As values have types
-- types have kinds
-- and kinds can be "typed"
-- f has kind Type -> Type
class Functor' (f :: Type -> Type) where
    fmap' :: (a -> b) -> (f a -> f b)

data Optional a = Ok a | Null


instance Functor Optional where
    fmap f = \case
        Ok a  -> Ok . f $ a
        Null  -> Null

instance Functor' [] where
    fmap' _ [] = []
    fmap' f (x:xs) = f x : fmap' f xs


instance Functor' ((,) a :: Type -> Type) where
    fmap' :: (c -> d) -> ((a,c) ->(a,d))
    fmap' f (x,y) = (x, f y)

newtype Fun a b = Fun (a -> b)

-- Spooky.
instance Functor' (Fun a) where
    -- Show it satisfies the functor laws.
    fmap' :: (b -> c) -> (Fun a b -> Fun a c)
    fmap' = undefined
