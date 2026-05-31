{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
{-# LANGUAGE ImpredicativeTypes #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE EmptyCase #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeAbstractions #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}


module C3.Ex1 where

import Text.Parsec hiding (token)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe
import Data.List (intercalate)
import Data.Functor
import GHC.TypeLits
import Data.Singletons
import GHC.TypeLits.Singletons
import Data.Singletons.TH
import Data.Singletons.Decide
import Data.Ord.Singletons
import GHC.TypeNats
import C3.Patterns qualified as PU
import C3.Patterns hiding (type (<))
import Data.String (IsString(..))
import C3.Matchers
import Control.Monad.Except
import Control.Monad.IO.Class
import System.Random (randomIO)
import Control.Monad.State

type Parser a = Parsec String () a

data Dinero = MkDinero Double deriving Show

instance Num Dinero where
    fromInteger = MkDinero . fromInteger
    MkDinero a + MkDinero b = MkDinero $ a + b
    MkDinero a - MkDinero b = MkDinero $ a - b
    MkDinero a * MkDinero b = MkDinero $ a * b
    abs (MkDinero a) = MkDinero $ abs a
    signum (MkDinero a) = MkDinero $ signum a



keywords :: [String]
keywords = []


-- | Reserved (expression/type) operators
reservedOperators :: [String]
reservedOperators = []


instance (u ~ ()) => IsString (Parser u)  where
  fromString str
    | str `elem` keywords = keyword str
    | str `elem` reservedOperators
      = token (string str *> notFollowedBy (choice $ (void . string) <$> ["+","-","*","/"]) )
    | otherwise           = void $ token (string str)


data JSonValue
    = JString String
    | JNum   Double
    | JObject JSon
    | JArray [JSonValue]
    | JBool Bool
    | JNull
instance Show JSonValue where
    show (JString s) = show s
    show (JNum n) = show n
    show (JObject o) = show o
    show (JArray a) = "[" ++ intercalate ", " (map show a) ++ "]"
    show (JBool b) = if b then "true" else "false"
    show JNull = "null"


newtype JSon = JSon (Map String JSonValue)

instance Show JSon where
    show (JSon m) = "{" ++ intercalate ", " (map showPair (Map.toList m)) ++ "}"
        where
        showPair (k, v) = show k ++ ": " ++ show v

pJSon :: Parsec String () JSon
pJSon = between (char '{') (char '}')
    $ f <$> (( (,)
        <$> (pWhiteSpace *> pString <* pWhiteSpace <* char ':')
        <*> pJSonValue
    ) `sepBy` char ',')
    where
        f :: [(String,JSonValue)] -> JSon
        f = JSon . Map.fromList

pJSonValue :: Parsec String () JSonValue
pJSonValue = pWhiteSpace *>
    ( pStringValue
    <|> pNumberValue
    <|> pObjectValue
    <|> pArrayValue
    <|> pBoolValue
    <|> pNullValue
    ) <* pWhiteSpace
    where
    pStringValue, pNumberValue, pObjectValue, pArrayValue, pBoolValue, pNullValue :: Parsec String () JSonValue

    pStringValue = JString <$> pString
    pNumberValue = JNum    <$> pNumber
    pObjectValue = JObject <$> pJSon
    pArrayValue  = JArray . fmap JObject <$>
        between (char '[') (char ']')  (pJSon `sepBy` (char ',' *> spaces'))
    pBoolValue   = JBool   <$> ( (string "true" $> True) <|> (string "false" $> False))
    pNullValue   = JNull <$ string "null"

-- pure x <* f a = x <$ f a = fmap (const x) (f a)



-- Alternative ~ Monoid for Applicatives
pWhiteSpace :: Parsec String () ()
-- pWhiteSpace =
--     (
--         (char ' ' <|> char '\r' <|> char '\n' <|> char '\t') *> pWhiteSpace
--     ) <|> pure ()
pWhiteSpace = skipMany $ (choice . fmap char) " \r\n\t"

spaces' :: Parsec String () ()
spaces' = pWhiteSpace


-- f <$> fa = pure f <*> fa



pNumber :: Parsec String () Double
pNumber = f <$> pSign <*> pDigits <*> pFrac <*> pExp
    where
    pSign :: Parsec String () (Maybe Char)
    pSign = optionMaybe $ char '-'

    pDigits :: Parsec String () String
    pDigits = many1 digit

    pFrac :: Parsec String () (Maybe String)
    pFrac = optionMaybe $  (:) <$> char '.' <*> many1 digit

    pExp :: Parsec String () (Maybe String)
    pExp
        = optionMaybe
        $ g
        <$> (char 'e' <|> char 'E')
        <*> optionMaybe (char '-' <|> char '+')
        <*> many1 digit

    g :: Char -> Maybe Char -> String -> String
    g e mSign digits
        = (e : maybe "" (:[]) mSign) <> digits

    f :: Maybe Char -> String -> Maybe String -> Maybe String -> Double
    f mSign intPart mFrac mE
        = read
        $  maybe "" pure mSign
        <> intPart
        <> fromMaybe "" mFrac
        <> fromMaybe "" mE


-- (*>) :: f a -> f b -> f b
-- a *> b = pure const <*> a <*> b
--
-- (<*) :: f a -> f b -> f a
-- a <* b = pure (flip const) <*> a <*> b

--pure :: a -> [a]

pString :: Parsec String () String
pString = between (char '"') (char '"') middle where
    middle :: Parsec String () String
    middle = fmap concat . many $ (pure <$> noneOf ['"','\\']) <|>
        ( (:)
        <$> char '\\'
        <*> ( pure <$> (choice . fmap char) ['"','\\','/','b','f','r','n','t']
            <|> ( (:) <$> char 'u' <*> count 4 hexDigit)
            )
        )


data Expr = Add Expr Term | Minus Expr Term
data Term = Mul Term Val | Div Term Val
data Val  = ValDouble Double | Paren Expr

data family EPrec (n :: Natural)

type Inf  =  0xffffffffffffffff
type Atom = Inf
type PostfixPrec = 0xfffffffffffffffe
type PrefixPrec  = 0xfffffffffffffffd
type ExprPrec    = 0

data instance EPrec 6 where
    PPLus :: forall n. (SingI n, (n > 6) ~ True)
        => EPrec 6 -> EPrec n -> EPrec 6
    PMinus :: forall n. (SingI n, (n > 6) ~ True)
        => EPrec 6 -> EPrec n -> EPrec 6
    OfHigher6 :: forall n. (SingI n, (n > 6) ~ True)
        => EPrec n -> EPrec 6


data instance EPrec 7 where
    PMul :: forall n. (SingI n, (n > 7) ~ True)
        => EPrec 7 -> EPrec n -> EPrec 7
    PDiv ::  forall n. (SingI n, (n > 7) ~ True)
        => EPrec 7 -> EPrec n -> EPrec 7
    OfHigher7 :: forall n. (SingI n, (n > 7) ~ True)
        => EPrec n -> EPrec 7

data instance EPrec PrefixPrec where
    PNeg :: EPrec PrefixPrec -> EPrec PrefixPrec
    OfHigherPrefixPrec :: forall n. (SingI n, (n > PrefixPrec) ~ True )
        => EPrec n -> EPrec PrefixPrec

data instance EPrec Atom where
    PNum  :: Double -> EPrec Atom
    PParen :: forall n. (SingI n) => EPrec n -> EPrec Atom
    PRandom :: EPrec Atom

data instance EPrec ExprPrec where
    OfHigher0 :: forall n. (SingI n, (n > 0) ~ True)
        => EPrec n -> EPrec ExprPrec

pParse0 :: Parsec String () (EPrec ExprPrec)
pParse0 = fmap OfHigher0 . precedence $
    sops InfixL
        [ PPLus  <$ "+" <* spaces
        , PMinus <$ "-" <* spaces
        ] |-<
    sops InfixL
        [ PMul <$ "*" <* spaces
        , PDiv <$ "/"
        ] |-<
    sops Prefix
        [ PNeg <$  "-"
        ] |-<
    Atom pAtom
pExpr :: Parsec String () (EPrec ExprPrec)
pExpr = pParse0

pAtom :: Parsec String () (EPrec Atom)
pAtom
    =   (PNum <$> (pNumber <* spaces) )
    <|> (PParen <$> parens pParse0)
    <|> (PRandom <$ "random")

instance (SingI n', SingI n, (n' > n) ~ True) => EPrec n' PU.< EPrec n where
    -- upcast :: EPrec n' -> EPrec n
    upcast p = case () of
        _ | Just Refl <- matches @0 (sing @n) -> OfHigher0 p
        _ | Just Refl <- matches @6 (sing @n) -> OfHigher6 p
        _ | Just Refl <- matches @7 (sing @n) -> OfHigher7 p
        _ | Just Refl <- matches @PrefixPrec (sing @n) -> OfHigherPrefixPrec p
        _ | Just Refl <- matches @Atom (sing @n) -> error "caso imposible"
        _ -> error  "error: upcast solo funciona con expresiones de precedencia: 0,6,7, y  prefijas."

    downcast t = case () of

        _ | Just Refl <- matches @0 (sing @n) -> case t of
            OfHigher0 f -> genericDowncast f
        _ | Just Refl <- matches @6 (sing @n) -> case t of
            OfHigher6 f -> genericDowncast f
            _ -> Nothing
        _ | Just Refl <- matches @7 (sing @n) -> case t of
            OfHigher7 f -> genericDowncast f
            _ -> Nothing
        _ | Just Refl <- matches @PrefixPrec (sing @n) -> case t of
            OfHigherPrefixPrec f -> genericDowncast f
            _ -> Nothing
        _ -> error "upcast solo funciona con expresiones de precedencia: 0,6,7, y prefijas"
        where
          genericDowncast :: forall x. (SingI x)
            =>  EPrec x -> Maybe (EPrec n')
          genericDowncast f = withKnownNat (sing @x) $ case sCompare' @n' @x of
              EQ' -> withEqRefl @n' @x $ Just f
              LT' -> Just $ upcast  @(EPrec x) @(EPrec n') f
              GT' -> downcast @(EPrec n') @(EPrec x) f

instance (SingI n) => Show (EPrec n) where
    showsPrec p = case () of
        _ | Just Refl <- matches @0 (sing @n) -> \case
            OfHigher0 e -> showsPrec p e
        _ | Just Refl <- matches @6 (sing @n) -> \case
            PPLus  a b -> showParen (p > 6) $ showsPrec 6 a . showString " + " . showsPrec 7 b
            PMinus  a b -> showParen (p > 6) $ showsPrec 6 a . showString " - " . showsPrec 7 b
            OfHigher6 e -> showsPrec p e

        _ | Just Refl <- matches @7 (sing @n) -> \case
            PMul  a b -> showParen (p > 7) $ showsPrec 7 a . showString " * " . showsPrec 8 b
            PDiv  a b -> showParen (p > 7) $ showsPrec 7 a . showString " / " . showsPrec 8 b
            OfHigher7 a -> showsPrec p a

        () | Just Refl <- matches @PrefixPrec (sing @n) -> \case
            PNeg  e -> showParen (p > 10) $ showString "-" . shows e
            OfHigherPrefixPrec e -> showsPrec p e
        () | Just Refl <- matches @Atom (sing @n) -> \case
            PNum n -> shows n
            PParen e -> showParen True $ shows e
        _ -> error "precedencia no definida"

eval :: forall n m. (SingI n, Eff m)
    => EPrec n  -> m (EPrec 0)
eval = case () of
    _ | Just Refl <- matches @0 (sing @n) -> \case
        OfHigher0 e -> eval e
    _ | Just Refl <- matches @6 (sing @n) -> \case
        e@(PPLus l r)  -> evalBinOp (+) (show e) l r
        e@(PMinus l r) -> evalBinOp (-) (show e) l r
        OfHigher6 e -> eval e
    _ | Just Refl <- matches @7 (sing @n) -> \case
        e@(PMul l r)  -> evalBinOp (*) (show e) l r
        e@(PDiv l r) -> evalBinOp' (/) (show e) (\_ n -> if n == 0 then throwError "Division por 0" else pure ()) l r
        OfHigher7 e -> eval e
    _ | Just Refl <- matches @PrefixPrec (sing @n) -> \case
        (PNeg l)  -> (downcast <$> eval l) >>= \case
            Just (PNum l') -> pure . OfHigher0 . PNum $ (-l')
            _              -> error $ show l <> " no es un numero"
        OfHigherPrefixPrec e -> eval e
    _ | Just Refl <- matches @Atom (sing @n) -> \case
        PNum a -> pure . OfHigher0 $ PNum a
        PRandom -> (OfHigher0 . PNum) <$> random'
        PParen e -> eval e
    _ -> error ""


type Eff m =
    ( MonadError String m
    , RandomGenerator m
    , Monad m
    )

{-
type AEff m =
    (Eff m
    , ...
    )
-}

evalBinOp' :: (Eff m,SingI nl, SingI nr)
    => (Double -> Double -> Double)
    -> String
    -> (Double -> Double -> m ())
    -> EPrec nl
    -> EPrec nr
    -> m (EPrec 0)
evalBinOp' op sError guard l r = do
    l' <- downcast <$> eval l
    r' <- downcast <$> eval r
    case (l',r') of
        (Just (PNum a), Just (PNum b))
            -> do
                guard a b
                pure . OfHigher0 . PNum $ a `op` b
        (Just (PNum a), _)
            -> throwError
                $ "error el lado derecho de "
                <> sError
                <> " no es un numero"
        (_,Just (PNum b))
            -> throwError
                $ "error el lado izquierdo de "
                <> sError
                <> " no es un numero"
        _ -> throwError
                $ "error, ningun operando de "
                <> sError
                <> " es un numero"

evalBinOp :: (Eff m,SingI nl, SingI nr)
    => (Double -> Double -> Double)
    -> String
    -> EPrec nl
    -> EPrec nr
    -> m (EPrec 0)
evalBinOp f e l r = evalBinOp' f e (\_ _ -> pure ()) l r


class RandomGenerator m where
    -- 0<= random' <= 1
    random' :: m Double

newtype EvalMonad a = MkEvalMonad
    { runEvalMonad :: ExceptT String IO a
    } deriving newtype
        ( Functor
        , Applicative
        , Monad
        , MonadIO
        , (MonadError String)
        )

instance RandomGenerator EvalMonad where
    random' = randomIO @Double @EvalMonad

newtype EvalMonad2 a = MkEvalMonad2
    { runEvalMonad2 :: StateT (Double,Double) (Either String) a
    } deriving newtype
        ( Functor
        , Applicative
        , Monad
        , (MonadError String)
        , (MonadState (Double,Double))
        )

instance RandomGenerator EvalMonad2 where
    random' = do
        (l1,l2) <- get
        let res  = sin l1 + sin l2 * 2
        put (res * 47, l1*l2)
        pure res



-- ReaderT

runInterpreter :: String -> IO ()
runInterpreter s = case runParser (fully pExpr) () "" s of
    Left pError -> print pError
    Right pe    -> runExceptT (runEvalMonad (eval pe)) >>= \case
        Left xError -> putStrLn xError
        Right res   -> print res

runInterpreter2 :: (Double,Double) -> String -> IO (Double,Double)
runInterpreter2 si s = case runParser (fully pExpr) () "" s of
    Left pError -> print pError >> pure si
    Right pe    -> case runStateT (runEvalMonad2 (eval pe)) si of
        Left xError -> putStrLn xError >> pure si
        Right (res,sf)   -> print res >> pure sf
