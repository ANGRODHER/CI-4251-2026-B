{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE PolyKinds           #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE TypeOperators       #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE InstanceSigs        #-}
{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE MonoLocalBinds      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

module C3.Matchers where

import Data.Kind (Type)
import Prelude.Singletons
import Data.Singletons.Decide
import GHC.TypeLits.Singletons
import GHC.TypeLits
import Unsafe.Coerce (unsafeCoerce)

class Matchable (k :: Type) where
  match :: forall (a :: k) (b :: k). SingI a => Sing b -> Maybe (b :~: a)

instance Matchable Symbol where
  match :: forall (a :: Symbol) (b :: Symbol). SingI a => Sing b -> Maybe (b :~: a)
  match b@SSymbol = case sing @a of
    SSymbol -> sameSymbol b (SSymbol @a)

instance Matchable Natural where
  match :: forall (a :: Nat) (b :: Nat). SingI a => Sing b -> Maybe (b :~: a)
  match b@SNat = case sing @a of
    SNat -> sameNat b (SNat @a)

matches :: forall {k} (a :: k) (b :: k).
  (Matchable k, SingI a) => Sing b -> Maybe (b :~: a)
matches = match @k

withSucc :: forall n k. SingI n => (SingI (n Prelude.Singletons.+ 1) => k) -> k
withSucc f = case sing @n of
  SNat -> case sing @n %+ sing @1 of
    SNat -> f

-- class Satisfies (k :: Type) where
--   satisfies :: forall {a1 :: k} (a :: k).
--     SingI a => Sing (TyFun a1 Bool) -> Maybe (TyFun a )


data Ordering' a b where
  EQ' :: forall a b. ((a == b) ~ True, (b == a) ~ True) => Ordering' a b
  LT' :: forall a b. ((a <  b) ~ True, (b > a ) ~ True) => Ordering' a b
  GT' :: forall a b. ((a >  b) ~ True, (b < a ) ~ True) => Ordering' a b

sCompare' :: forall {k} (a :: k) (b :: k). (SOrd k, SingI a, SingI b) => Ordering' a b
sCompare' = case (sing @a %== sing @b, sing @a %< sing @b, sing @a %> sing @b) of
  (STrue,_,_) -> downEQ' @a @b $ EQ'
  (_,STrue,_) -> downLT' @a @b $ LT'
  (_,_,STrue) -> downGT' @a @b $ GT'
  _           -> error "impossible case. SOrd imposes a total order."

downLT' :: forall {k} (a :: k) (b :: k) r. (SOrd k, SingI a, SingI b, (a < b) ~ True) => (( (b > a) ~ True) => r) -> r
downLT' f = case sing @b %> sing @a  of
    STrue  -> f
    SFalse -> error "error in reversing LT'"

downGT' :: forall {k} (a :: k) (b :: k) r. (SOrd k, SingI a, SingI b, (a > b) ~ True) => (( (b < a) ~ True) => r) -> r
downGT' f = case sing @b %< sing @a  of
    STrue  -> f
    SFalse -> error "error in reversing GT'"

downEQ' :: forall {k} (a :: k) (b :: k) r. (SOrd k, SingI a, SingI b, (a == b) ~ True) => (( (b == a) ~ True) => r) -> r
downEQ' f = case sing @b %== sing @a  of
    STrue  -> f
    SFalse -> error "error in reversing EQ'"


eqToRefl :: (a == b) ~ True => a :~: b
eqToRefl = unsafeCoerce trivialRefl

trivialRefl :: () :~: ()
trivialRefl = Refl

withEqRefl :: forall a b r. (a == b) ~ True => ((a ~ b) => r) -> r
withEqRefl f = case eqToRefl @a @b of
  Refl -> f
