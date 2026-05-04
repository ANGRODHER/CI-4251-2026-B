{-# LANGUAGE GADTs #-}

module C2.Ex1 where

import Data.Kind
import Data.Foldable (traverse_)

-- As values have types
-- types have kinds
-- and kinds can be "typed"
-- f has kind Type -> Type
class Functor' (f :: Type -> Type) where
    fmap' :: (a -> b) -> (f a -> f b)

data Optional a = Ok a | Null


instance Functor' Optional where
    fmap' f = \case
        Ok a  -> Ok . f $ a
        Null  -> Null

instance Functor' [] where
    fmap' _ [] = []
    fmap' f (x:xs) = f x : fmap' f xs


instance Functor' ((,) a :: Type -> Type) where
    fmap' :: (c -> d) -> ((a,c) ->(a,d))
    fmap' f (x,y) = (x, f y)


-- Spooky.
instance Functor' (Fun a) where
    -- Show it satisfies the functor laws.
    fmap' :: (b -> c) -> (Fun a b -> Fun a c)
    fmap' (f :: b -> c)
        = \(Fun (g :: a -> b))
        -> Fun $ (f . g :: a -> c)

instance Functor' ((->) a) where
    fmap' :: (b -> c) -> ((a -> b) -> (a -> c))
    fmap' f g = f . g


xs :: [[Int]]
xs = [[1,2,3],[4,5],[6,7,8,9]]

ys :: [[String]]
ys = (fmap' . fmap') show $ xs

zs :: [[[String]]]
zs = (fmap' . fmap' . fmap') show $ [xs]


xs0 :: [(String,Int)]
xs0 = [("Daniel",10),("Enzo",20),("Baralt",30)]

ys0 :: [(String,String)]
ys0 = (fmap' . fmap') show $ xs0

newtype Compose (f :: Type -> Type) (g :: Type -> Type) (a :: Type)
    = Compose {getCompose :: f (g a)}

exampleCompose :: Compose [] ((,) String) Int
exampleCompose = Compose [("Daniel",10),("Enzo",20),("Baralt",30)]

instance (Functor' f, Functor' g) =>  Functor' (Compose f g) where
    fmap' :: (a -> b) -> (Compose f g a) -> (Compose f g b)
    fmap' h (Compose fgx) = Compose $ (fmap' . fmap') h fgx

data State s a = State {runState :: s -> (s,a)}

    -- ((->) s)
    -- (,) s

instance Functor' (State s) where
    fmap' :: (a -> b) -> (State s a) -> (State s b)
    -- fmap' (f :: a -> b) (State (g :: s -> (s,a))) = State
    --     $ \s ->
    --     let (s',a) = g s
    --         b      = f a
    --     in (s',b)
    fmap' f (State g) = State . getCompose $ fmap' f (Compose g)

newtype Fun a b  = Fun  (a -> b)
newtype EFun b a = EFun (a -> b)

-- DFun c b a = DFun ((a -> c) -> b)

-- class ContravariantFunctor (f :: Type -> Type) where
--     contraMap :: (b -> a) -> (f a -> f b)
--     fmap      :: (a <- b) -> (f a -> f b)

-- instance Functor' (EFun b) where
--     fmap' :: (a -> c) -> (EFun b a) -> (EFun b c)
--     -- fmap' :: (a -> c) -> (a -> b) -> (c -> b)
--     fmap' (f :: a -> c) (EFun (g :: a -> b) ) = EFun
--         $ \c :: c
--         ->
--

data Cont r a = Cont ((a -> r) -> r)

instance Functor' (Cont r) where
    fmap' = undefined

-- x :: IO String
-- x = getLine
--
-- ^id = Ok $ \x -> x
--
-- Just f
-- Just x
--
-- Just (f x)
--
--
-- f x
-- Just (f x)
--
-- [[f x]] = [[ (\f -> f x) f ]]
--
-- f :: IO (String -> String)
-- f = do
--     putStrLn "what's yer name? "
--     s <- getLine
--     pure $ \s -> "Hello " <> s <> " I'm padding with: " <> s
--
-- -- aux = fmap' f x


class Functor' f => Applicative' (f :: Type -> Type) where
    pure' :: a -> f a

    (<*$>) :: f (a -> b) -> f a -> f b

instance Applicative' Optional where
    pure' :: a -> Optional a
    pure' = Ok

    (Ok f) <*$> (Ok x) = Ok $ f x
    Null   <*$> (Ok _) = Null
    (Ok _) <*$> Null   = Null

sqrt' :: Double -> Double -> Double -> Optional (Double,Double)
sqrt' a b c = pure' f <*$> pure' (-b) <*$> det a b c <*$> den a
    where
    f :: Double -> Double -> Double -> (Double,Double)
    f x y z = ( (x + y) / z, (x - y) / z)

    det :: Double -> Double -> Double -> Optional Double
    det a b c = let d = b ** 2 - 4*a*c in if d >= 0 then Ok d else Null

    den :: Double -> Optional Double
    den a = if a == 0 then Null else Ok (2*a)

instance Applicative' [] where
    pure' a = [a]
    fs <*$> xs = [f x | f <- fs, x <- xs]

triplets :: [(Int,Int,Int)]
triplets = pure (,,) <*$> [1,2,3] <*$> [1,2,3] <*$> [1,2,3]

-- for x in xs
--     for y in ys
--         fos z in zs
--             f(x,y,z)

newtype ZipList a = ZipList {getZipList :: [a]}

instance Functor' ZipList where
    fmap' f (ZipList xs) = ZipList $ fmap' f xs

instance Applicative' ZipList where
    pure' a = ZipList $ repeat a
    ZipList fs <*$> ZipList xs = ZipList $ zipWith ($) fs xs


-- a ->
-- [(+1),(*2),(*5)]
-- [ 1,   2   ,3  ,4,5]
-- [2,4,15]
--
--
-- pure' id <*$> [1,2,3,4,5]
-- [id,id,id,id,id,id,id,id] <*$> [1,2,3,4,5]
-- [1]

dotProduct :: Num a => [a] -> [a] -> a
dotProduct xs ys
    = sum . getZipList
    $ pure' (*)
    <*$> ZipList xs
    <*$> ZipList ys


instance Monoid a => Applicative' ((,) a) where
    pure' :: b -> (a, b)
    pure' = (mempty,)

    (<*$>) :: (a, (b -> c)) -> (a, b) -> (a, c)
    (x, f) <*$> (y, a) = (x <> y, f a)

instance Applicative' ((->) a) where
    pure' :: b -> (a -> b)
    pure' = const

    (<*$>) :: (a -> (b -> c)) -> (a -> b) -> (a -> c)
    f <*$>  g = \x -> f x (g x)


weird :: Num a =>  a -> a
weird = pure' (+) <*$> (*2) <*$> (+10)
        --     +
        -- *2   +10
        --
        --          +
        -- (*2) (3)   (+10) (3)
        --
        --   +
        -- 6   13
        --
        --  19

weird3 :: (Num a) => a -> a
weird3
    = pure (+)
    <*> (*2)
    <*> (pure (*)
        <*> (*4)
        <*> (+8)
        )

instance Applicative' (State s) where
    pure' :: a -> State s a
    pure' x = State $ \s -> (s,x)

    (<*$>) :: State s (a -> b) -> State s a -> State s b
    (State sf) <*$> (State sx) = State $ \s ->
        let (s',f)  = sf s
            (s'',x) = sx s'
        in (s'',f x)

instance Functor (State s) where
    fmap = fmap'

instance Applicative (State s) where
    pure = pure'
    (<*>) = (<*$>)

-- f ^x ]] = [[ ^($ x) f
put :: s -> State s ()
put s = State $ \_ -> (s,())

get :: State s s
get = State $ \s -> (s,s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> (f s,())

gets :: (s -> a) -> State s a
gets f = State $ \s -> (s, f s)

execState :: State s a -> s -> s
execState (State f) s = let (s',_) = f s  in s'


mean :: forall a. Fractional a => [a] -> a
mean xs = normalize $ traverse_ step xs `execState` (0,0)
    where
    normalize :: (a,Int) -> a
    normalize (x,y) = x / fromIntegral y

    step :: a -> State (a,Int) ()
    step x = modify $ \(a,n) -> (a + x, n + 1)

instance (Applicative' f, Applicative' g) => Applicative' (Compose f g)

-- GADT
-- Existencial Types
data Ap (f :: Type -> Type) a where
    Pure    :: a -> Ap f a
    (:<*>:) :: Ap f (a -> b) -> f a -> Ap f b

data Ap' (f :: Type -> Type) a where
    Pure'    :: a -> Ap' f a
    (:<**>:) :: f (a -> b) -> Ap' f a -> Ap' f b

data Ap'' (f :: Type -> Type) a where
    Pure''    :: a -> Ap'' f a
    (:<***>:) :: Ap'' f (a -> b) -> Ap'' f a -> Ap'' f b
