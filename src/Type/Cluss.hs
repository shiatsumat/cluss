{-# LANGUAGE TypeFamilies, PolyKinds, DataKinds, TypeOperators, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, UndecidableInstances, FlexibleContexts, ConstraintKinds, RankNTypes #-}

module Type.Cluss (
    -- * Clusses
    In(..)
    -- * Type Patterns
  , Type, Unary, Binary, Ternary, Quaternary, Quinary, Senary, type (<|)
    -- * Instance Products
  , AllOf, AllOf'(..)
    -- * Constraints
    -- ** Basic Constraints
  , This, Pure, Is
    -- ** Overlaying Contraints
  , type (>+<), type (>++<), type (>+++<), type (>++++<), type (>+++++<), type (>++++++<)
    -- ** Bonding Contraints
  , type (>|<), type (>||<), type (>|||<), type (>||||<), type (>|||||<)
    -- * Helpers
    -- ** For Identical Constructors
  , and, andI, and1, and2, and3, and4, and5, and6, none, projI
    -- ** For Function Constructors
  , andF, andF1, andF2, andF3, andF4, andF5, andF6, noneF, projF
    ) where

import Prelude hiding (and)
import GHC.Exts

data Look_At_Head
data Look_At_Tail a
data No_I_Don't_Have_That

data Type (a :: k)
data (a :: k) <| (p :: l)
type Unary (a :: i -> k) (s :: i -> Constraint) = a <| s
type Binary (a :: i -> i' -> k) (s :: i -> i' -> Constraint) = a <| s
type Ternary (a :: i -> i' -> k) (s :: i -> i' -> i'' -> Constraint) = a <| s
type Quaternary (a :: i -> i' -> i'' -> i''' -> k) (s :: i -> i' -> i'' -> i''' -> Constraint) = a <| s
type Quinary (a :: i -> i' -> i'' -> i''' -> i'''' -> k) (s :: i -> i' -> i'' -> i''' -> i'''' -> Constraint) = a <| s
type Senary (a :: i -> i' -> i'' -> i''' -> i'''' -> i''''' -> k) (s :: i -> i' -> i'' -> i''' -> i'''' -> i''''' -> Constraint) = a <| s

-- |'This' is used to create a recursion.
--  When used in the type list (the second parameter) of 'In', with @'>+<', '>++<', ... '>|<', '>||<', ...@, 'This' will work as the @`In` as@ itself.
--  There is basically no limit of recursion depth, but GHC has a fixed-depth recursion stack for safety, so you may need to increase the stack depth with @-fcontext-stack=N@.
class True ~ False => This (a :: k)
class Pure (a :: i)
instance Pure a
class (s a, s' a) => (>+<) s s' a
instance (s a, s' a) => (>+<) s s' a
class (s a b, s' a b) => (>++<) s s' a b
instance (s a b, s' a b) => (>++<) s s' a b
class (s a b c, s' a b c) => (>+++<) s s' a b c
instance (s a b c, s' a b c) => (>+++<) s s' a b c
class (s a b c d, s' a b c d) => (>++++<) s s' a b c d
instance (s a b c d, s' a b c d) => (>++++<) s s' a b c d
class (s a b c d e, s' a b c d e) => (>+++++<) s s' a b c d e
instance (s a b c d e, s' a b c d e) => (>+++++<) s s' a b c d e
class (s a b c d e f, s' a b c d e f) => (>++++++<) s s' a b c d e f
instance (s a b c d e f, s' a b c d e f) => (>++++++<) s s' a b c d e f
class (s a, s' b) => (>|<) s s' a b
instance (s a, s' b) => (>|<) s s' a b
class (s a b, s' c) => (>||<) s s' a b c
instance (s a b, s' c) => (>||<) s s' a b c
class (s a b c, s' d) => (>|||<) s s' a b c d
instance (s a b c, s' d) => (>|||<) s s' a b c d
class (s a b c d, s' e) => (>||||<) s s' a b c d e
instance (s a b c d, s' e) => (>||||<) s s' a b c d e
class (s a b c d e, s' f) => (>|||||<) s s' a b c d e f
instance (s a b c d e, s' f) => (>|||||<) s s' a b c d e f
type Is = (~)

infixl 7 <|
infixl 8 >|<, >||<, >|||<, >||||<, >|||||<
infixl 9 >+<, >++<, >+++<, >++++<, >+++++<, >++++++<

type family Where (as :: [*]) (a :: k) :: * where
    Where (Type a ': as) a = Look_At_Head
    Where (a <| p ': as) (a b) = Look_At_Head
    Where (a <| p ': as) (a b c) = Look_At_Head
    Where (a <| p ': as) (a b c d) = Look_At_Head
    Where (a <| p ': as) (a b c d e) = Look_At_Head
    Where (a <| p ': as) (a b c d e f) = Look_At_Head
    Where (a <| p ': as) (a b c d e f g) = Look_At_Head
    Where (b ': as) a = Look_At_Tail (Where as a)
    Where '[] a = No_I_Don't_Have_That

type family Modify (this :: k -> Constraint) (a :: k -> Constraint) :: k -> Constraint where
    Modify this This = this
    Modify this (s >+< s') = Modify this s >+< Modify this s'
    Modify this s = s
type family Modify2 (this :: k -> Constraint) (a :: k -> k' -> Constraint) :: k -> k' -> Constraint where
    Modify2 this (s >++< s') = Modify2 this s >++< Modify2 this s'
    Modify2 this (s >|< s') = Modify this s >|< Modify this s'
    Modify2 this s = s
type family Modify3 (this :: k -> Constraint) (a :: k -> k' -> k'' -> Constraint) :: k -> k' -> k'' -> Constraint where
    Modify3 this (s >+++< s') = Modify3 this s >+++< Modify3 this s'
    Modify3 this (s >||< s') = Modify2 this s >||< Modify this s'
    Modify3 this s = s
type family Modify4 (this :: k -> Constraint) (a :: k -> k' -> k'' -> k''' -> Constraint) :: k -> k' -> k'' -> k''' -> Constraint where
    Modify4 this (s >++++< s') = Modify4 this s >++++< Modify4 this s'
    Modify4 this (s >|||< s') = Modify3 this s >|||< Modify this s'
    Modify4 this s = s
type family Modify5 (this :: k -> Constraint) (a :: k -> k' -> k'' -> k''' -> k'''' -> Constraint) :: k -> k' -> k'' -> k''' -> k'''' -> Constraint where
    Modify5 this (s >+++++< s') = Modify5 this s >+++++< Modify5 this s'
    Modify5 this (s >||||< s') = Modify4 this s >||||< Modify this s'
    Modify5 this s = s
type family Modify6 (this :: k -> Constraint) (a :: k -> k' -> k'' -> k''' -> k'''' -> k''''' -> Constraint) :: k -> k' -> k'' -> k''' -> k'''' -> k''''' -> Constraint where
    Modify6 this (s >++++++< s') = Modify6 this s >++++++< Modify6 this s'
    Modify6 this (s >|||||< s') = Modify5 this s >|||||< Modify this s'
    Modify6 this s = s

type AllOf as = AllOf' as as
data family AllOf' (ts :: [*]) (as :: [*]) (f :: k -> *)
data instance AllOf' ts (Type a ': as) f = And (f a) (AllOf' ts as f)
data instance AllOf' ts (a <| p ': as) f = And1 (forall b. Modify (In ts) p b => f (a b)) (AllOf' ts as f)
data instance AllOf' ts (a <| p ': as) f = And2 (forall b c. Modify2 (In ts) p b c => f (a b c)) (AllOf' ts as f )
data instance AllOf' ts (a <| p ': as) f = And3 (forall b c d. Modify3 (In ts) p b c d => f (a b c d)) (AllOf' ts as f)
data instance AllOf' ts (a <| p ': as) f = And4 (forall b c d e. Modify4 (In ts) p b c d e => f (a b c d e)) (AllOf' ts as f)
data instance AllOf' ts (a <| p ': as) f = And5 (forall b c d e f'. Modify5 (In ts) p b c d e f' => f (a b c d e f')) (AllOf' ts as f)
data instance AllOf' ts (a <| p ': as) f = And6 (forall b c d e f' g. Modify6 (In ts) p b c d e f' g => f (a b c d e f' g)) (AllOf' ts as f)
data instance AllOf' ts '[] f = None

infixr 0 `And`, `And1`, `And2`, `And3`, `And4`, `And5`, `And6`

class In (as :: [*]) (a :: k) where
    proj :: AllOf as f -> f a
instance In' (Where as a) as as a => In as a where
    proj = proj' (undefined :: Where as a)
class In' (n :: *) (ts :: [*]) (as :: [*]) (a :: k) where
    proj' :: n -> AllOf' ts as f -> f a
instance In' Look_At_Head ts (Type a ': as) a where
    proj' _ (And x _) = x
instance Modify (In ts) p b => In' Look_At_Head ts (a <| p ': as) (a b) where
    proj' _ (And1 x _) = x
instance Modify2 (In ts) p b c => In' Look_At_Head ts (a <| p ': as) (a b c) where
    proj' _ (And2 x _) = x
instance Modify3 (In ts) p b c d => In' Look_At_Head ts (a <| p ': as) (a b c d) where
    proj' _ (And3 x _) = x
instance Modify4 (In ts) p b c d e => In' Look_At_Head ts (a <| p ': as) (a b c d e) where
    proj' _ (And4 x _) = x
instance Modify5 (In ts) p b c d e f => In' Look_At_Head ts (a <| p ': as) (a b c d e f) where
    proj' _ (And5 x _) = x
instance Modify6 (In ts) p b c d e f g => In' Look_At_Head ts (a <| p ': as) (a b c d e f g) where
    proj' _ (And6 x _) = x
instance In' n ts as a => In' (Look_At_Tail n) ts (Type (b :: k) ': as) (a :: k) where
    proj' _ (And _ xs) = proj' (undefined :: n) xs
instance In' n ts as a => In' (Look_At_Tail n) ts ((b :: i -> k) <| (p :: i -> Constraint) ': as) (a :: k) where
    proj' _ (And1 _ xs) = proj' (undefined :: n) xs
instance In' n ts as a => In' (Look_At_Tail n) ts ((b :: i -> i' -> k) <| (p :: i -> i' -> Constraint) ': as) (a :: k) where
    proj' _ (And2 _ xs) = proj' (undefined :: n) xs
instance In' n ts as a => In' (Look_At_Tail n) ts ((b :: i -> i' -> i'' -> k) <| (p :: i -> i' -> i'' -> Constraint) ': as) (a :: k) where
    proj' _ (And3 _ xs) = proj' (undefined :: n) xs
instance In' n ts as a => In' (Look_At_Tail n) ts ((b :: i -> i' -> i'' -> i''' -> k) <| (p :: i -> i' -> i'' -> i''' -> Constraint) ': as) (a :: k) where
    proj' _ (And4 _ xs) = proj' (undefined :: n) xs
instance In' n ts as a => In' (Look_At_Tail n) ts ((b :: i -> i' -> i'' -> i''' -> i'''' -> k) <| (p :: i -> i' -> i'' -> i''' -> i'''' -> Constraint) ': as) (a :: k) where
    proj' _ (And5 _ xs) = proj' (undefined :: n) xs
instance In' n ts as a => In' (Look_At_Tail n) ts ((b :: i -> i' -> i'' -> i''' -> i'''' -> i''''' -> k) <| (p :: i -> i' -> i'' -> i''' -> i'''' -> i''''' -> Constraint) ': as) (a :: k) where
    proj' _ (And6 _ xs) = proj' (undefined :: n) xs

newtype Id a = Id {unId :: a}
and, andI :: a -> AllOf' ts as Id -> AllOf' ts (Type a ': as) Id
and x y = And (Id x) y
andI = and
and1 :: (forall b. Modify (In ts) p b => a b) -> AllOf' ts as Id -> AllOf' ts (a <| p ': as) Id
and1 x y = And1 (Id x) y
and2 :: (forall b c. Modify2 (In ts) p b c => a b c) -> AllOf' ts as Id -> AllOf' ts (a <| p ': as) Id
and2 x y = And2 (Id x) y
and3 :: (forall b c d. Modify3 (In ts) p b c d => a b c d) -> AllOf' ts as Id -> AllOf' ts (a <| p ': as) Id
and3 x y = And3 (Id x) y
and4 :: (forall b c d e. Modify4 (In ts) p b c d e => a b c d e) -> AllOf' ts as Id -> AllOf' ts (a <| p ': as) Id
and4 x y = And4 (Id x) y
and5 :: (forall b c d e f. Modify5 (In ts) p b c d e f => a b c d e f) -> AllOf' ts as Id -> AllOf' ts (a <| p ': as) Id
and5 x y = And5 (Id x) y
and6 :: (forall b c d e f g. Modify6 (In ts) p b c d e f g => a b c d e f g) -> AllOf' ts as Id -> AllOf' ts (a <| p ': as) Id
and6 x y = And6 (Id x) y
none :: AllOf' ts '[] Id
none = None
projI :: In as a => AllOf as Id -> a
projI = unId . proj

newtype Func b a = Func {unFunc :: a -> b}
andF :: (a -> t) -> AllOf' ts as (Func t) -> AllOf' ts (Type a ': as) (Func t)
andF x y = And (Func x) y
andF1 :: (forall b. Modify (In ts) p b => a b -> t) -> AllOf' ts as (Func t) -> AllOf' ts (a <| p ': as) (Func t)
andF1 x y = And1 (Func x) y
andF2 :: (forall b c. Modify2 (In ts) p b c => a b c -> t) -> AllOf' ts as (Func t) -> AllOf' ts (a <| p ': as) (Func t)
andF2 x y = And2 (Func x) y
andF3 :: (forall b c d. Modify3 (In ts) p b c d => a b c d -> t) -> AllOf' ts as (Func t) -> AllOf' ts (a <| p ': as) (Func t)
andF3 x y = And3 (Func x) y
andF4 :: (forall b c d e. Modify4 (In ts) p b c d e => a b c d e -> t) -> AllOf' ts as (Func t) -> AllOf' ts (a <| p ': as) (Func t)
andF4 x y = And4 (Func x) y
andF5 :: (forall b c d e f. Modify5 (In ts) p b c d e f => a b c d e f -> t) -> AllOf' ts as (Func t) -> AllOf' ts (a <| p ': as) (Func t)
andF5 x y = And5 (Func x) y
andF6 :: (forall b c d e f g. Modify6 (In ts) p b c d e f g => a b c d e f g -> t) -> AllOf' ts as (Func t) -> AllOf' ts (a <| p ': as) (Func t)
andF6 x y = And6 (Func x) y
noneF :: AllOf' ts '[] (Func t)
noneF = None
projF :: In as a => AllOf as (Func t) -> (a -> t)
projF = unFunc . proj
