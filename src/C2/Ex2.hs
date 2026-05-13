module C2.Ex2 where


import Data.Kind
import Text.Read (readMaybe)


data State (s :: Type) (a :: Type)
  = MkState
    { runState :: s -> (s,a)
    }


instance Functor (State s) where
  fmap :: (a -> b)
    -> State s a
    -> State s b
  {-fmap f sa = do
    a <- sa
    pure $ f a
  -}
  fmap f sa = pure f <*> sa

instance Applicative (State s) where
  pure :: a -> State s a
  pure x =  MkState $ \s ->
    (s,x)

  (<*>) :: State s (a -> b)
    -> State s a
    -> State s b
  sf <*> sa = do
    f <- sf
    a <- sa
    pure $ f a


--State s :: Type -> Type
--Monad   :: Type -> Type
instance Monad (State s) where
  (>>=) :: State s a
    -- (s -> (s,a))
    -> (a -> State s b)
    -- a -> s -> (s,b)
    -> State s b
  MkState sa >>= k = MkState $ \s
    -> let (s',a )  = sa s
      in runState (k a) s'


putState :: s -> State s ()
putState s
  = MkState $ const (s,())

getState :: State s s
getState = MkState $ \s -> (s,s)

modifyState :: (s -> s) -> State s ()
{-modifyState f
  = getState
  >>= putState . f
-}
modifyState f = do
  s <- getState
  let next = f s
  putState next


evalState :: State s a -> s -> a
evalState st s = snd $ runState st s

execState :: State s a -> s -> s
execState st s = fst $ runState st s

data E
  = Mul
  | Div
  | Sum
  | Res
  deriving (Eq,Show)

--(3 + 4) × (5 + 6) becomes 3 4 + 5 6 + ×
--77

empilarNum :: Double -> State ([Double],[E]) ()
empilarNum n = modifyState $ \(nums,ops) -> (n:nums,ops)

empilarOp ::  E -> State ([Double],[E]) ()
empilarOp e = modifyState $ \(nums,ops) -> (nums,e:ops)

ops :: [(String,E)]
ops =
  [ ("+",Sum)
  , ("-",Res)
  , ("*",Mul)
  , ("/",Div)
  ]

parse :: String -> State ([Double],[E]) ()
parse op   | Just e <- lookup op ops    = empilarOp e
parse numS | Just num <- readMaybe numS = empilarNum num
parse _  = error "numero no valido"

binOps :: [(E, Double -> Double -> Double)]
binOps =
  [ (Sum, (+))
  , (Res, (-))
  , (Mul, (*))
  , (Div, (/))
  ]


interpret :: State ([Double],[E]) Double
interpret = do
  s@(nums,ops) <- getState
  case (nums,ops) of
    ([num],[]) -> pure num
    (x:y:res,op:ops)
      | Just f <- lookup op binOps -> do
          let z = f x y
          let newState = (z:res, ops)
          putState $ newState
          interpret
    otherwise -> error $ "syntax error: " <> show s


test :: IO ()
test = do
  {-
  let s = ([],[])
  print $ execState  (parse "+") s
  print $ flip execState s $ do
    parse "+"
    parse "*"
    parse "3.14"
    parse "/"
  -- 3 4 + 5 6 + ×
  print $ flip runState s $ do
    traverse parse ["*","+","6","5","+","4","3"]
    interpret
  -}
  let sf :: Validation String (Int -> Int) = Err "1"
  let sa :: Validation String Int          = Err "2"
  print $ do
    f <- sf
    a <- sa
    pure $ f a
  print $ sf <*> sa

  pure ()


data Validation e a = Err e  | Ok    a deriving Show
--data Either     e a = Left e | Right a

instance Functor (Validation e) where
  fmap f (Ok a)  = Ok $ f a
  fmap _ (Err e) = Err e

instance Monoid e => Applicative (Validation e) where
  pure a = Ok a
  Err e <*> Err e' = Err $ e <> e'
  Ok f  <*> Ok a   = Ok  $ f a
  Err e <*> Ok _   = Err e
  Ok _  <*> Err e  = Err e

-- >>= es unico por tipo
-- <*> puede NO ser unico
--con >>= puedo <*>


instance Monoid e => Monad (Validation e) where
  (>>=)
    :: Validation e a
    -> (a -> Validation e b)
    -> Validation e b
  Ok a >>= f = f a
  Err e >>= _ = Err e


{-
traverse :: (Traversable t, Applicative f)
  => (a     ->     f       b )
  -- String -> (State [E]) ()
  ->   t a
  -- [String]
  -> f (t b)
  -- (State [E]) [()]
-}

-- data MySet a = MySet (Set a)

{-
instance Functor MySet where
  fmap :: Ord b => (a -> b) -> MySet a -> MySet b
  fmap f (MySet s) = undefined
-}

data MyState s m a = MyState
  {
    runMyState :: m (s -> (s, a))
  }

data MyState' s m a = MyState'
  {
    runMyState' :: (s -> m (s, a))
  }


instance Functor m => Functor (MyState' s m) where
  fmap = undefined
instance Applicative m => Applicative (MyState' s m) where
  pure = undefined
  (<*>) = undefined

instance Monad m => Monad (MyState' s m) where
  (>>=) :: MyState' s m a
    -> (a -> MyState' s m b)
    -> MyState' s m b
  MyState' sma >>= f = MyState' $ \s -> do
    (s,a) <- sma s
    let MyState' smb = f a
    smb s

--es imposible
{-
instance Monad m => Monad (MyState s m) where
  (>>=) :: MyState s m a
    -> (a -> MyState s m b)
    --
    -> MyState s m b
  MyState msa >>= f = MyState $ do
    -- contexto m
    sa :: s -> (s, a) <- msa
    MyState s m (\s -> m (s,b) )
    MyState s m (\s ->(s,b) )
    MyState s m (\s -> (s,b) )
    pure $ \s ->
        let (s',a) = sa s
            MyState (msb :: _) = f a
        in do
          sb <- msb
          pure $ sb s'
-}
