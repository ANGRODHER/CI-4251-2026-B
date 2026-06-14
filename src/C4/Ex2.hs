{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FunctionalDependencies #-}

module C4.Ex2 where

import Data.Functor.Identity
import Data.Map qualified as M
import Data.Map (Map)
import Control.Lens
import Data.Char (toUpper)
import Data.Tree
import Control.Monad.State

data Piece = Pawn | Knight | Bishop | Castle |  Queen | King deriving (Eq,Ord)

newtype Position = Position {_getPoisiton :: (Char,Int)}

data Color = Black | White deriving (Eq,Ord)

data CPiece = CPiece
    { _getPiece :: Piece
    , _getColor :: Color

    }

newtype Board = Board {_getBoard :: Map (Piece,Color) Position}

data CastlingState = CastlingState
    { _queenSideCastle :: Bool
    , _kingSideCastle  :: Bool
    }

data Move = Move
    { _getMovePiece :: CPiece
    , _from         :: Position
    , _to           :: Position

    }

data AdditionalState = AdditionalState
    { _getCastlingState     :: CastlingState
    , _lastMovedPiece       :: Maybe CPiece
    , _turnsWithoutCaptures :: Int
    , _moves                :: [Move]
    }

data BoardState = BoardState
    { _getBoard'       :: Board
    , _currentTurn     :: Color
    , _additionalState :: AdditionalState
    }

noCaptureTurn ::  BoardState -> BoardState
noCaptureTurn bs@(BoardState {_additionalState=as})
    = bs{_additionalState = (_additionalState bs){_turnsWithoutCaptures = _turnsWithoutCaptures as + 1}}

getTurnWithoutCaptures :: BoardState -> Int
getTurnWithoutCaptures bs = bs & _turnsWithoutCaptures . _additionalState

-- (&) = flip ($)


data ItemType = Weapon | Potion | Armour | Misc deriving (Show, Eq)

data Item = Item
  { _iname   :: String
  , _itype  :: ItemType
  , _iweight :: Double
  , _ivalue  :: Int
  , _ilevel  :: Int
  } deriving (Show)

data Bag = Bag { _hero :: String, _items :: [Item] } deriving (Show)


$(makeLenses ''Item)
$(makeLenses ''Bag)

sampleBag :: Bag
sampleBag = Bag "Aria"
  [ Item "Iron Sword"   Weapon  2.5 80  1
  , Item "Health Potion" Potion 0.3 30  0
  , Item "Chain Mail"   Armour  8.0 200 0
  , Item "Torch"        Misc    0.5 5   0
  ]


test :: IO ()
test = do
    let x :: Item = head . _items $ sampleBag
    print $ x
        & ivalue +~ 120
        & ilevel %~ (\lvl -> if lvl >= 50 then 1 else lvl + 20)

        -- +~ 5 === %~ (\x -> x + 5)



    pure ()


data Gate = Gate
    { _open :: Bool
    , _oilTemp :: Float
    } deriving (Eq,Show)
makeLenses ''Gate
data Army = Army
    { _archers :: Int
    , _knights :: Int
    } deriving (Eq,Show)
makeLenses ''Army
data Kingdom = Kingdom
    { _name :: String
    , _army :: Army
    , _gate :: Gate
    } deriving (Eq,Show)
makeLenses ''Kingdom

duloc :: Kingdom
duloc = Kingdom
    { _name = "Duloc"
    , _army = Army
        { _archers = 22
        , _knights = 14
        }
    , _gate = Gate
        { _open = True
        , _oilTemp = 10.0
        }
    }

goalA :: Kingdom
goalA = Kingdom
    { _name = "Duloc: a perfect place"
    , _army = Army
        { _archers = 22
        , _knights = 42
        }
    , _gate = Gate
        { _open = False
        , _oilTemp = 10.0
        }
    }

goalB :: (String,Kingdom)
goalB = ("Duloc: Home", Kingdom
    { _name = "Dulocinstein"
    , _army = Army
        { _archers = 17
        , _knights = 26
        }
    , _gate = Gate
        { _open = True
        , _oilTemp = 100.0
        }
    }
    )

self x = x

test2 :: IO ()
test2 = do
    -- let spine = army . archers . ...
    print $ duloc
        & name %~ (<> ": a perfect place")
        & army . knights .~ 42
        & gate . open .~ False
    print $ duloc
        & name %~ (<> "instein")
        & army . archers -~ 4
        & army . knights +~ (26 - 42)
        -- & army . (archers -~ 4 &&& knights +~ (26-42))
        & gate . oilTemp *~ 10
        & self %~ ("Duloc: Home",)
    print duloc
    print $ [duloc,goalA, snd goalB]
        --    ^     ^          ^
        ^.. folded . army . archers
    print $ (folded . army . archers) `sumOf` [duloc, goalA, snd goalB]
    print $ ["Hola ", "Como ", "estas"] ^.. folded
    print $ ["Hola ", "Como ", "estas"] ^.. folded . folded
    print $ ("abc","def","ghi","ijk") ^.. each . to reverse
    print $ ("abc","def","ghi","ijk") ^.. _2 . to reverse
    print $ ("abc","def") ^.. _2 . to reverse
    print $ [1..5] & taking 3 traversed *~ 10
    print $ [1..5] & dropping 3 traversed *~ 10
    print $ [1..5] & traversed . filtered even *~ 10
    print $ "Hello World" & worded %~ (<> "!!!")
    print $ "Hello\nWorld" & lined %~ (<> "???")
    print $ ("T-Rex",(42,"Stegosaurus")) & beside id _2 %~ (<> "!!")

    pure ()


type Path = [String]
type Body = String
data Request = Post Path Body | Get Path | Delete Path deriving (Show,Eq)

_Post :: Prism' Request (Path, Body)
_Post = prism' embed match
    where
    embed (path,body)      = Post path body
    match (Post path body) = Just (path,body)
    match _                = Nothing

_Get :: Prism' Request Path
_Get = prism' embed match
    where
    embed = Get
    match (Get path ) = Just path
    match _           = Nothing

_Delete :: Prism' Request Path
_Delete = prism' embed match
    where
    embed = Delete
    match (Delete path ) = Just path
    match _              = Nothing



  --   a
  -- b   c
  --   d

data Temperature = Temperature
    { _location :: String
    , _celsius :: Float
    } deriving (Show)
makeLenses ''Temperature

fahrenheitToCelsius :: Float -> Float
fahrenheitToCelsius c = (c * 9/5) + 32

celsiusToFahrenheit:: Float -> Float
celsiusToFahrenheit f = (f - 32) * 5 / 9

fahrenheit :: Lens' Temperature Float
fahrenheit = lens getter setter
    where
    getter = celsiusToFahrenheit . view celsius
    setter temp f = temp & celsius .~ fahrenheitToCelsius f

fahrenheit' :: Iso' Float Float
fahrenheit' = iso celsiusToFahrenheit fahrenheitToCelsius

validateEmail :: String -> Either [String] String
validateEmail email
    | '@' `elem` email = Right email
    | otherwise        = Left ["Falta un '@' para: " <> email]

data Validation e a
    = Error e
    | Success a
    deriving (Eq,Show)

instance Functor (Validation e) where
    fmap f (Success a) = Success $ f a
    fmap f (Error e)   = Error e

instance (Monoid e) => Applicative (Validation e) where
    pure = Success
    (Error e) <*> (Error e') = Error $ e <> e'
    Success f <*> Success x  = Success $ f x
    Error e <*> _            = Error e
    _ <*> Error e            = Error e

validateEmail' :: String -> Validation [String] String
validateEmail' email
    | '@' `elem` email = Success email
    | otherwise        = Error ["Falta un '@' para: " <> email]


correos :: [String]
correos =
    [ "15-1129@usb.ve"
    , "20-2541@usb.ve"
    , "24-1147@usb.ve"
    ]

malo :: [String]
malo =
    [ "15-1129@usb.ve"
    , "20-2541usb.ve"
    , "24-1147usb.ve"
    ]

tree :: Tree Int
tree = Node 1
    [ Node 2
        [Node 4 []]
    , Node 3
        [Node 5 [], Node 6 []]

    ]

newtype Cycled a = Cycled [a] deriving Show
type role Cycled representational

type instance Index (Cycled a) = Int
type instance IxValue (Cycled a) = a



instance Ixed (Cycled a) where
    ix n f (Cycled xs) =  fmap (Cycled . pure) . f $ concat (repeat xs) !! n
         -- ^          ^
         --           [a]
         -- a -> f b


-- view myLens (set myLens newValue structure)
-- ==
-- newValue
--
--
-- set myLens (view myLens structure) structure
-- ==
-- structure
--
--
-- set myLens newValue (set myLens value structure)
-- ==
-- set myLens newValue structure
--

-- ["Hola ", "Como ", "estas"]
--   | ^  |  |   ^ |  |  ^  |
--
-- ["Hola ", "Como ", "estas"]
--   [^^^^^    ^^^^^    ^^^^^]

data Actor = Actor
    { _Aname :: String
    , _birthYear :: Int
    } deriving (Show, Eq)
makeLenses ''Actor

data TVShow = TVShow
    { _title :: String
    , _numEpisodes :: Int
    , _numSeasons :: Int
    , _criticScore :: Double
    , _actors :: [Actor]
    } deriving (Show, Eq)
makeLenses ''TVShow

howIMetYourMother :: TVShow
howIMetYourMother = TVShow
    { _title = "How I Met Your Mother"
    , _numEpisodes = 208
    , _numSeasons = 9
    , _criticScore = 83
    , _actors =
        [ Actor "Josh Radnor" 1974
        , Actor "Cobie Smulders" 1982
        , Actor "Neil Patrick Harris" 1973
        , Actor "Alyson Hannigan" 1974
        , Actor "Jason Segel" 1980
        ]
    }

buffy :: TVShow
buffy = TVShow
    { _title = "Buffy the Vampire Slayer"
    , _numEpisodes = 144
    , _numSeasons = 7
    , _criticScore = 81
    , _actors =
        [ Actor "Sarah Michelle Gellar" 1977
        , Actor "Alyson Hannigan" 1974
        , Actor "Nicholas Brendon" 1971
        , Actor "David Boreanaz" 1969
        , Actor "Anthony Head" 1954
        ]
    }
tvShows :: [TVShow]
tvShows =
    [ howIMetYourMother
    , buffy
    ]


-- (.)       :: (b -> c) -> (a        -> b) -> a -> c
-- (.) . (.) :: (b -> c) -> (a1 -> a2 -> b) -> a1 -> a2 -> c
-- (.) . (.) . (.)
--           :: (b -> c) -> (a1 -> a2 -> a3 -> b) -> a1 -> a2 -> a3 -> c

fmapDefault :: Traversable t => (a -> b) -> t a -> t b
fmapDefault f =    runIdentity . traverse (Identity . f)

foldMapDefault :: (Traversable t, Monoid m) => (a -> m) -> t a -> m
foldMapDefault f = getConst    . traverse (Const    . f)

-- data Const' a b = Const' a
--
-- instance Functor (Const' a) where
--     fmap _ (Const' a) = Const' a
--
-- instance (Monoid m) => Applicative (Const' m) where
--     pure = const $ Const' mempty
--     Const' a <*> Const' b = Const' (a <> b)over
--   :: ((a1 -> Identity b) -> a2 -> Identity c) -> (a1 -> b) -> a2 -> c
--
-- over
--   :: ((a -> Identity b) -> s -> Identity t) -> (a -> b) -> s -> t
--
-- type Setter s t a b = ((a -> Identity b) -> s -> Identity t)
-- type Lens s t a b = forall f. Applicative f => ((a -> f b) -> s -> f t)
--
--
-- over
--     :: Setter s t a b -> (a -> b) -> s -> t


data MBinTree a = MNode a (MBinTree a) (MBinTree a) | Leaf
    deriving (Show,Eq)

instance Cons (MBinTree a) (MBinTree b) (a,MBinTree a) (b,MBinTree b) where
    -- _Cons :: Prism' (MBinTree a) (a,MBinTree a)
    _Cons = prism embed match
        where
        embed :: ((b,MBinTree b),MBinTree b) -> MBinTree b
        embed ((a,l),r) = MNode a l r

        match :: MBinTree a -> Either (MBinTree b) ((a,MBinTree a), MBinTree a)
        match Leaf = Left Leaf
        match (MNode a l r) = Right ((a, l),r)

a0, a1, a2, a3 :: MBinTree Int
a0 = Leaf
a1 = MNode 1 a0 a0
a2 = MNode 1
    ( MNode 2 a0 a0 )
    ( MNode 3 a0 a0)
a3 = MNode 1
    ( MNode 2
        (MNode 4 a0 a0)
        Leaf
    )
    ( MNode 3
        Leaf
        (MNode 5 a0 a0)
    )

data VentaCel = VC
    { _total   :: Double
    , _sales   :: [Double]
    , _taxRate :: Double
    } deriving (Show,Eq)

makeLenses ''VentaCel

someCalculation :: StateT VentaCel IO ()
someCalculation = do
    total .= 10
    total += 15
    total *= 3.14
    pure ()

sc :: IO ()
sc = runStateT someCalculation (VC 3 [] 5) >> pure ()


data Person = Person
    { _personName :: String
    } deriving (Eq,Show)

data Pet = Pet
    { _petName :: String
    } deriving (Eq,Show)

class HasName s a | s -> a where
    name' :: Lens' s a

instance HasName Person String where
    name' = lens getter setter
        where
        getter = _personName
        setter _ = Person

instance HasName Pet String where
    name' = lens getter setter
        where
        getter = _petName
        setter _ = Pet

-- f :: (MonadReader r m, ) => ...
