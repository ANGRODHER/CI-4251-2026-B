module Test1.Sol where

import Data.List (intercalate, (!?), inits, tails)
import Control.Monad
import Control.Applicative
import Control.Monad.Free
import Debug.Trace
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State
import Control.Monad.Reader
import Data.Maybe
import Control.Monad.Except
import Data.Traversable
import Data.Foldable

------------------------
--- 1.
------------------------

newtype AccountId = AccountId String deriving (Show, Eq)
newtype User = User String deriving (Show, Eq)
data Error = InsufficientFunds | AccountNotFound deriving (Show, Eq)

data ATMF a
    = CheckBalance AccountId (Int -> a)
    | Deposit AccountId Int a
    | Withdraw AccountId Int (Either Error a)
    | GetUser AccountId (Maybe User -> a)

instance Functor ATMF where
    fmap = undefined


checkBalance :: AccountId -> Free ATMF Int
checkBalance acc = undefined

deposit :: AccountId -> Int -> Free ATMF ()
deposit acc amount = undefined

withdraw :: AccountId -> Int -> Free ATMF (Either Error ())
withdraw acc amount = undefined

getUser :: AccountId -> Free ATMF (Maybe User)
getUser acc = undefined

transfer :: AccountId -> AccountId -> Int -> Free ATMF (Either Error ())
transfer from to amount = undefined

transfer' :: AccountId -> AccountId -> Int -> Either Error (Free ATMF ())
transfer' from to amount = undefined

accountIds :: Free ATMF a -> [AccountId]
accountIds = undefined

newtype BankState = MkBankState (Map AccountId (Int, User))

interpret :: Free ATMF a -> BankState -> (Either Error a, BankState)
interpret = undefined

-------------------------
--- 2.
-------------------------

type Weight = Int
newtype Graph a = Graph [(a,[(Weight,a)])]

paths :: forall a. Ord a => a -> Graph a -> [[a]]
paths = undefined

--------------------------
--- 3.
--------------------------


data MyExceptT e m a = MyExceptT { runMyExceptT :: m (Either e a) }

instance Functor m => Functor (MyExceptT e m) where
    fmap = undefined

instance Applicative m => Applicative (MyExceptT e m) where
    pure = undefined
    (<*>) = undefined

instance Monad m => Monad (MyExceptT e m) where
    (>>=) = undefined

--------------------------
--- 4.
--------------------------

data Piece = Pawn | Knight | Bishop | Rook | Queen | King deriving (Eq)
data Color = White | Black deriving (Eq)
data Position = Position
    { rank :: Int
    , file :: Char
    } deriving (Eq,Ord)
data Move = Move
    { from :: Position
    , to   :: Position
    } deriving (Eq)

newtype Board = MkBoard (Map Position Piece)

data BoardState' a = BoardState'
    { board       :: Board
    , turn        :: Color
    , customState :: a
    }

type AdditionalState = ()
type BoardState = BoardState' AdditionalState

data BoardError
    = InvalidMove { reasonIM :: String }
    deriving (Show)


initialState :: BoardState
initialState = undefined

toString :: Board -> String
toString = undefined

move :: Move -> StateT BoardState (ExceptT BoardError IO) ()
move = undefined

parseMove :: String -> Either BoardError Move
parseMove = undefined

playGame :: StateT BoardState (ExceptT BoardError IO) ()
playGame = undefined

playChess :: IO ()
playChess = runExceptT (runStateT playGame initialState) >>= \case
    Left err -> putStrLn $ "Game ended with error: " ++ show err
    Right _ -> putStrLn "Game ended successfully"
