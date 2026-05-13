{-# LANGUAGE GADTs #-}

module C2.Ex where


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

newtype Fun a b = Fun (a -> b)

-- Spooky.
instance Functor' (Fun a) where
    -- Show it satisfies the functor laws.
    fmap' :: (b -> c) -> (Fun a b -> Fun a c)
    fmap' f = \(Fun g) -> Fun $ f . g

instance Functor' ((->) a) where
    fmap' :: (b -> c) -> ((a -> b) -> (a -> c))
    fmap' f g = f . g


data Reader r a = Reader (r -> a)

instance Functor' (Reader r) where
    fmap' :: (a -> b) -> (Reader r a -> Reader r b)
    fmap' f = \(Reader g) -> Reader $ f . g

data Compose (f :: Type -> Type) (g :: Type -> Type) a = Compose {getCompose :: f (g a)}

instance (Functor' f, Functor' g) => Functor' (Compose f g) where
    fmap' :: (a -> b) -> (Compose f g a -> Compose f g b)
    fmap' f = \(Compose x) -> Compose $ (fmap' . fmap') f $ x

data State s a = State {runState :: s -> (s,a)}

instance Functor' (State s) where
    fmap' :: forall a b. (a -> b) -> (State s a -> State s b)
    -- fmap' f (State g) = State . getCompose $ fmap' @(Compose ((->) s) ((,) s)) f (Compose g)
    fmap' f (State g) = State $ \s -> let (s', a) = g s in (s', f a)


data EFun a b = EFun (b -> a)

data Cont r a = Cont ((a -> r) -> r)

instance Functor' (Cont r) where
    fmap' :: (a -> b) -> (Cont r a -> Cont r b)
    fmap' f (Cont (g :: (a -> r) -> r ))
        = Cont
        $  \(k :: (b -> r))
        ->  let h :: (a -> r) = k . f
                r :: r = g h
            in r



---------------------
-- Applicative
---------------------

class Functor' f => Applicative' (f :: Type -> Type) where
    pure' :: a -> f a
    pure' = undefined

    (<*$>) :: f (a -> b) -> f a -> f b
    (<*$>) = undefined

instance Applicative' Optional where
    pure' = Ok
    (Ok f) <*$> (Ok a) = Ok $ f a
    _ <*$> _ = Null


sqrt' :: Double -> Double -> Double -> Optional (Double, Double)
sqrt' a b c = pure' f <*$> pure' (-b) <*$> det a b c <*$> den a
    where
    (/?) :: Double -> Double -> Optional Double
    _ /? 0 = Null
    num /? den = Ok $ num / den

    den :: Double -> Optional Double
    den 0 = Null
    den a = Ok $ 2 * a

    det :: Double -> Double -> Double -> Optional Double
    det a b c = let d = b**2 - 4 * a * c in if d >= 0 then Ok d else Null

    pm :: Double -> Double -> (Double, Double)
    pm x y = (x+y,x-y)

    f :: Double -> Double -> Double  -> (Double,Double)
    f x y z = ((x+y) / z, (x-y) / z)

instance Applicative' [] where
    pure' x = [x]
    fs <*$> xs = [f x | f <- fs, x <- xs]


triplets :: [(Int, Int, Int)]
triplets = pure' (,,) <*$> [1,2,3] <*$> [1,2,3] <*$> [1,2,3]

newtype ZipList a = ZipList {getZipList :: [a]}

instance Functor' ZipList where
    fmap' f (ZipList xs) = ZipList $ [f x | x <- xs]

instance Applicative' ZipList where
    pure' x = ZipList $ repeat x
    (ZipList fs) <*$> (ZipList xs) = ZipList $ zipWith ($) fs xs

instance Functor ZipList where
    fmap f (ZipList xs) = ZipList $ [f x | x <- xs]

instance Applicative ZipList where
    pure x = ZipList $ repeat x
    (ZipList fs) <*> (ZipList xs) = ZipList $ zipWith ($) fs xs

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

weird :: (Num a) => a -> a
weird = (*) <*$> (+1)

weird2 :: (Num a) => a -> a
weird2 = pure (+) <*> (*2) <*> (+10)

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
    pure' x = State $ \s -> (s, x)

    (<*$>) :: State s (a -> b) -> State s a -> State s b
    (State f) <*$> (State g) = State $ \s ->
        let (s', h) = f s
            (s'', a) = g s'
        in (s'', h a)

instance Applicative (State s) where
    pure = pure'
    (<*>) = (<*$>)

instance Functor (State s) where
    fmap = fmap'


put :: s -> State s ()
put s = State $ \_ -> (s, ())

get :: State s s
get = State $ \s -> (s, s)

modify :: (s -> s) -> State s ()
modify f = State $ \s -> (f s, ())

gets :: (s -> a) -> State s a
gets f = State $ \s -> (s, f s)

execState :: State s a -> s -> s
execState (State f) s = fst $ f s

mean :: forall a. (Fractional a) => [a] -> a
mean xs = normalize $ traverse_ step xs `execState` (0,0)
    where
    normalize :: (a, Int) -> a
    normalize (x, n) = x / fromIntegral n
    step :: a -> State (a, Int) ()
    step x = modify $ \(s, n) -> (s + x, n + 1)

-- mapState :: (s -> s') -> State s a -> State s' a
-- mapState f (State g) = State $ \s' -> let (s, a) = g (f s') in (f s, a)

-- mapState :: (s' -> s) -> State s a -> State s' a
-- mapState f (State g) = State $ \s' -> let (s, a) = g (f s') in (s, a)



instance (Applicative' f, Applicative' g) => Applicative' (Compose f g) where
    pure' :: a -> Compose f g a
    pure' x = Compose $ pure' (pure' x)

    (<*$>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose fgh) <*$> (Compose fgx) = Compose $ fmap' (<*$>) fgh <*$> fgx

f1 :: Compose ((->) String) ((,) String) Int
f1 = Compose $ \s -> (show $ read s +1,read s)

f2 :: State String Int
f2 = State $ \s -> (show $ read s +1,read s)

f1' :: String -> (String,Int)
f1' = getCompose $ pure' (\x y z -> x + y + z) <*$> f1 <*$> f1 <*$> f1

f2' :: String -> (String,Int)
f2' = runState   $  pure' (\x y z -> x + y + z) <*$> f2 <*$> f2 <*$> f2

testState :: IO ()
testState = do
  let (s1x,f1x) = f1' "5"
  let (s2x,f2x) = f2' "5"
  putStrLn $ "(" <> s1x <> ", " <> show f1x <> ")"
  putStrLn $ "(" <> s2x <> ", " <> show f2x <> ")"




data Ap (f :: Type -> Type) a where
    Pure :: a -> Ap f a
    -- (:<*>:) :: Ap f (a -> b) -> f a ->  Ap f b
    (:<*>:) :: Ap f (a -> b) -> f a ->  Ap f b

infixl 4 :<*>:

instance Functor' f => Functor' (Ap f) where
    fmap' :: (b -> c) -> (Ap f b -> Ap f c)
    fmap' h (Pure x) = Pure $ h x
    fmap' h (g :<*>: x) = (fmap' . fmap') h g :<*>: x

instance Functor' f => Applicative' (Ap f) where
    pure' :: a -> Ap f a
    pure' = Pure


runAp :: Applicative' f => Ap f a -> f a
runAp (Pure x) = pure' x
runAp (g :<*>: x) = runAp g <*$> x


weird2Ap :: (Num a) => Ap ((->) a) a
weird2Ap = Pure (+)
    :<*>: (*2)
    :<*>: (+10)

weird3A :: (Num a) => Ap ((->) a) a
weird3A = Pure (+)
    :<*>: (*2)
    :<*>: runAp ((Pure (*)
        :<*>: (*4))
        :<*>: (+8)
        )

helloPerson :: IO String
helloPerson = pure greet
    <*> (putStrLn "gimme your name! " >> getLine)
    <*> (putStrLn "gimme your age! " >> readLn)
    where
    greet :: String -> Int -> String
    greet name age = "Hello " <> name <> ", you are " <> show age <> " years old!"

class Applicative' f => Monad' f where
    (>>>=) :: f a -> (a -> f b) -> f b


instance Monad' Optional where
    (>>>=) :: Optional a -> (a -> Optional b) -> Optional b
    Null >>>= _ = Null
    Ok a >>>= f = f a


instance Functor Optional where
    fmap = fmap'
instance Applicative Optional where
    pure = pure'
    (<*>) = (<*$>)
instance Monad Optional where
    (>>=) = (>>>=)

sqrt'' :: Double -> Double -> Double -> Optional (Double, Double)
sqrt'' a b c = do
    let d = b**2 - 4 * a * c
    det <- if d >= 0 then Ok d else Null
    den <- if a == 0 then Null else Ok (2 * a)
    pure ( (-b + det) / den, (-b - det) / den )

instance Monad' [] where
    (>>>=) :: [a] -> (a -> [b]) -> [b]
    xs >>>= f = concat $ fmap f xs

increasing :: Int -> [(Int,Int,Int)]
increasing n = do
    x <- [1..n]
    y <- [1..x]
    z <- [1..y]
    pure (x,y,z)

instance Monoid a => Monad' ((,) a) where
    (>>>=) :: (a, b) -> (b -> (a, c)) -> (a, c)
    (x, a) >>>= f = let (y, c) = f a in (x <> y, c)

instance Monad' ((->) r) where
    (>>>=) :: (r -> b) -> (b -> (r -> c)) -> (r -> c)
    f >>>= g = \x -> g (f x)  x
