{-# LANGUAGE TypeFamilies, PolyKinds, DataKinds, TypeOperators, MultiParamTypeClasses, FlexibleInstances, ScopedTypeVariables, UndecidableInstances, FlexibleContexts, ConstraintKinds, RankNTypes #-}

module Type.Cluss (
    -- * Clusses
    In(..)

    -- * Type Patterns
    {-| Type patterns are used in the type list (first parameter) of 'In'.
        Each type pattern corresponds to the head of an instance declaration for a type class, namely, @instance ... where@. -}
  , Type, type (<|), Unary, Binary, Ternary, Quaternary, Quinary, Senary

    -- * Instance Products
  , AllOf, AllOf'(..)

    -- * Constraint Combinators
    {-| Constraint combinators are used in the second parameter of '<|', 'Unary', 'Binary', ..., 'Senary'.
        Note that each combinator is kind-polymorphic. -}

    -- ** Basic Combinators
  , This, Pure, Is

    -- ** Combinators for Overlaying Contraints
  , type (>+<), type (>++<), type (>+++<), type (>++++<), type (>+++++<), type (>++++++<)

    -- ** Combinators for Bonding Contraints
  , type (>|<), type (>||<), type (>|||<), type (>||||<), type (>|||||<)

    -- * Helpers

    -- ** Helpers for Identical Constructors
  , AllOfI, AllOfI', andI, andI1, andI2, andI3, andI4, andI5, andI6, noneI, projI

    -- ** Helpers for Function Constructors
  , AllOfF, AllOfF', andF, andF1, andF2, andF3, andF4, andF5, andF6, noneF, projF

    -- * Examples

    -- ** Example1: Hello
    -- $hello

    -- ** Example2: Printf
    -- $printf

    -- ** Example3: Monad
    -- $monad

    ) where

import Prelude hiding (and)
import GHC.Exts

-- $hello
-- Let's begin with a basic example.
--
-- >type Hellos = [
-- >    Type String,
-- >    Type Int,
-- >    Type Double,
-- >    Unary [] Show,
-- >    Quaternary (,,,) (This >|< This >||< This >|||< This)]
-- >hello :: In Hellos a => a -> String
-- >hello = projF (
-- >    (\s -> "hello, " ++ s) `andF`
-- >    (\n -> "Mr. " ++ show n) `andF`
-- >    (\x -> show (x / 2) ++ " times two") `andF`
-- >    (\xs -> concatMap ((++", ") . show) xs ++ "period") `andF1`
-- >    (\(x,y,z,w) ->
-- >        hello x ++ " and " ++ hello y ++ " and " ++
-- >        hello z ++ " and " ++ hello w) `andF4`
-- >    noneF :: AllOfF Hellos String)
-- >
-- >main = putStrLn $
-- >    hello ("world", 42 :: Int, 3.14 :: Double, [True, False])
--
-- This is the result.
--
-- > hello, world and Mr. 42 and 1.57 times two and True, False, period

-- $printf
-- With a recursive cluss,
-- you can easily make a function that can take a variable number of arguments.
-- The function below is a simplified C-style printf.
--
-- >type Printfs = [Binary (->) (Show >|< This), Type String]
-- >printf :: In Printfs a => String -> a
-- >printf s = projI (
-- >    (\x -> printf (go s (show x))) `andI2`
-- >    s `andI`
-- >    noneI :: AllOfI Printfs)
-- >  where
-- >    go ('@' : cs) t = t ++ cs
-- >    go (c : cs) t = c : go cs t
-- >    go [] t = error "there is no '@' any more!"
-- >
-- >main = putStrLn $
-- >    printf "@ good @ and @" 12 "men" True
--
-- This is the result.
--
-- >12 good "men" and True

-- $monad
-- Here is a more complex example.
-- When the type of a "cluss method" is complex, you generally have to create newtypes (like Bind and Return below).
--
-- >type Monads = [Type [], Unary (->) Pure, Unary (,) Monoid, Unary Wrap This]
-- >newtype Wrap m a = Wrap {unWrap :: m a}
-- >newtype Bind a b m = Bind {unBind :: m a -> (a -> m b) -> m b}
-- >newtype Return a m = Return {unReturn :: a -> m a}
-- >bind :: In Monads m => m a -> (a -> m b) -> m b
-- >bind = unBind $ proj (
-- >    Bind (\m k -> concatMap k m) `And`
-- >    Bind (\m k e -> k (m e) e) `And1`
-- >    Bind (\(a,x) k -> let (a',x') = k x in (a<>a',x')) `And1`
-- >    Bind (\m k -> Wrap (unWrap m `bind` (unWrap . k))) `And1`
-- >    None :: AllOf Monads (Bind a b))
-- >return' :: In Monads m => a -> m a
-- >return' = unReturn $ proj (
-- >    Return (: []) `And`
-- >    Return const `And1`
-- >    Return ((,) mempty) `And1`
-- >    Return (Wrap . return') `And1`
-- >    None :: AllOf Monads (Return a))
-- >infixl 1 `bind`
-- >
-- >main = print $
-- >    return' 1 `bind` replicate 3 `bind` (\n -> [n .. n+4])
--
-- This is the result.
--
-- >[1,2,3,4,5,1,2,3,4,5,1,2,3,4,5]

data Look_At_Head
data Look_At_Tail a
data No_I_Don't_Have_That

-- | The empty type @Type a@ is a type pattern.
-- For example, the type pattern @Type Int@ corresponds to @instance C Int where ...@ (@C@ is a type class).
-- Note that the type variable @a@ can be of any kind.
data Type (a :: k)
-- | The empty type @a <| p@ is a type pattern,
-- where @a@ is a type constructor, and @p@ is a constraint function for the type variables for the constructor 'a'.
-- For example, the type pattern @[] <| Show@ corresponds to @instance (Show a) => C [a] where ...@ (@C@ is a type class).
--
-- You can replace any of 'Unary', 'Binary', ..., 'Senary' with '<|',
-- but you can sometimes save the effort of annotating kinds
-- using 'Unary', 'Binary', ..., 'Senary' instead of '<|',
-- especially when using @PolyKinds@ extension,
-- because kinds of parameters are restricted in 'Unary', 'Binary', ..., 'Senary'.
data (a :: k) <| (p :: l)
-- | @a '<|' p@, with @a@ being of the kind @i -> k@ and @p@, @i -> 'Constraint'@.
type Unary (a :: i -> k) (p :: i -> Constraint) = a <| p
-- | @a '<|' p@, with @a@ being of the kind @i -> i' -> k@ and @p@, @i -> i' -> 'Constraint'@.
type Binary (a :: i -> i' -> k) (p :: i -> i' -> Constraint) = a <| p
-- | @a '<|' p@, with @a@ being of the kind @i -> i' -> i'' -> k@ and @p@, @i -> i' -> i'' -> 'Constraint'@.
type Ternary (a :: i -> i' -> k) (p :: i -> i' -> i'' -> Constraint) = a <| p
-- | @a '<|' p@, with @a@ being of the kind @i -> i' -> i'' -> i''' -> k@ and @p@, @i -> i' -> i'' -> i''' -> 'Constraint'@.
type Quaternary (a :: i -> i' -> i'' -> i''' -> k) (p :: i -> i' -> i'' -> i''' -> Constraint) = a <| p
-- | @a '<|' p@, with @a@ being of the kind @i -> i' -> i'' -> i''' -> i'''' -> k@ and @p@, @i -> i' -> i'' -> i''' -> i'''' -> 'Constraint'@.
type Quinary (a :: i -> i' -> i'' -> i''' -> i'''' -> k) (p :: i -> i' -> i'' -> i''' -> i'''' -> Constraint) = a <| p
-- | @a '<|' p@, with @a@ being of the kind @i -> i' -> i'' -> i''' -> i'''' -> i''''' -> k@ and @p@, @i -> i' -> i'' -> i''' -> i'''' -> i''''' -> 'Constraint'@.
type Senary (a :: i -> i' -> i'' -> i''' -> i'''' -> i''''' -> k) (p :: i -> i' -> i'' -> i''' -> i'''' -> i''''' -> Constraint) = a <| p

-- | 'This' creates a recursion.
-- In other words, 'This' will work as @'In' as@ itself
-- when used in the type list (first parameter) @as@ of 'In',
-- combined with 'Type', '<|', 'Unary', 'Binary', ..., 'Senary',
-- '>+<', '>++<', ..., '>++++++<',
-- '>|<', '>||<', ..., '>|||||<'.
--
-- Note that 'This' won't be expanded into @'In' as@
-- if the condition described above is not satisfied.
-- Internally, the expansion is executed by 'Modify', 'Modify2', ..., 'Modify6'.
--
-- The instance of 'This' itself can't be created
-- since the context @True~False@ will never be satisfied.
--
-- There is no predetermined limit of recursion depth,
-- but GHC has a fixed-depth recursion stack for safety,
-- so you may need to increase the stack depth with @-fcontext-stack=N@.
class True ~ False => This (a :: k)
-- | @'Pure' a@ is equivalent to the empty constraint @()@.
--
-- >Pure a == ()
class Pure (a :: i)
instance Pure a
-- |
-- >(p >+< q) a == (p a, q a)
class (p a, q a) => (>+<) p q a
instance (p a, q a) => (>+<) p q a
-- |
-- >(p >++< q) a b == (p a b, q a b)
class (p a b, q a b) => (>++<) p q a b
instance (p a b, q a b) => (>++<) p q a b
-- |
-- >(p >+++< q) a b c == (p a b c, q a b c)
class (p a b c, q a b c) => (>+++<) p q a b c
instance (p a b c, q a b c) => (>+++<) p q a b c
-- |
-- >(p >++++< q) a b c d == (p a b c d, q a b c d)
class (p a b c d, q a b c d) => (>++++<) p q a b c d
instance (p a b c d, q a b c d) => (>++++<) p q a b c d
-- |
-- >(p >+++++< q) a b c d e == (p a b c d e, q a b c d e)
class (p a b c d e, q a b c d e) => (>+++++<) p q a b c d e
instance (p a b c d e, q a b c d e) => (>+++++<) p q a b c d e
-- |
-- >(p >++++++< q) a b c d e f == (p a b c d e f, q a b c d e f)
class (p a b c d e f, q a b c d e f) => (>++++++<) p q a b c d e f
instance (p a b c d e f, q a b c d e f) => (>++++++<) p q a b c d e f
-- |
-- >(p >|< q) a b == (p a, q b)
class (p a, q b) => (>|<) p q a b
instance (p a, q b) => (>|<) p q a b
-- |
-- >(p >||< q) a b c == (p a b, q c)
class (p a b, q c) => (>||<) p q a b c
instance (p a b, q c) => (>||<) p q a b c
-- |
-- >(p >|||< q) a b c d == (p a b c, q d)
class (p a b c, q d) => (>|||<) p q a b c d
instance (p a b c, q d) => (>|||<) p q a b c d
-- |
-- >(p >||||< q) a b c d e == (p a b c d, q e)
class (p a b c d, q e) => (>||||<) p q a b c d e
instance (p a b c d, q e) => (>||||<) p q a b c d e
-- |
-- >(p >|||||< q) a b c d e f == (p a b c d e, q f)
class (p a b c d e, q f) => (>|||||<) p q a b c d e f
instance (p a b c d e, q f) => (>|||||<) p q a b c d e f

-- |
-- >(Is a) b == (a ~ b)
type Is a b = a ~ b

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

-- |@'AllOf' as f@ is a tuple that contains values of the type @f a@,
-- where @a@ can be any type that satisfies @In as a@.
-- Each value corresponds to each type pattern,
-- and the values in @'AllOf' as f@ must be in the same order as the type patterns in @as@.
-- 'And', 'And1', 'And2', ..., 'And6' are used to combine the values
-- and 'None' must be added at the end.
-- You have to use 'And' for @'Type' a@,
-- 'And1' for @'Unary' a p@, 'And2' for  @'Binary' a p@,
-- ..., 'And6' for @'Senary' a p@.
type AllOf as = AllOf' as as
data family AllOf' (ts :: [*]) (as :: [*]) (f :: k -> *)
data instance AllOf' ts (Type a ': as) f = And (f a) (AllOf' ts as f)
data instance AllOf' ts (Unary a p ': as) f = And1 (forall b. Modify (In ts) p b => f (a b)) (AllOf' ts as f)
data instance AllOf' ts (Binary a p ': as) f = And2 (forall b c. Modify2 (In ts) p b c => f (a b c)) (AllOf' ts as f )
data instance AllOf' ts (Ternary a p ': as) f = And3 (forall b c d. Modify3 (In ts) p b c d => f (a b c d)) (AllOf' ts as f)
data instance AllOf' ts (Quaternary a p ': as) f = And4 (forall b c d e. Modify4 (In ts) p b c d e => f (a b c d e)) (AllOf' ts as f)
data instance AllOf' ts (Quinary a p ': as) f = And5 (forall b c d e f'. Modify5 (In ts) p b c d e f' => f (a b c d e f')) (AllOf' ts as f)
data instance AllOf' ts (Senary a p ': as) f = And6 (forall b c d e f' g. Modify6 (In ts) p b c d e f' g => f (a b c d e f' g)) (AllOf' ts as f)
data instance AllOf' ts '[] f = None

infixr 0 `And`, `And1`, `And2`, `And3`, `And4`, `And5`, `And6`

-- | @'In' as@ is a /cluss/, where @as@ is a list of type patterns.
-- Normally, @as@ is concrete and does not contain any type variables, like @In [Binary (->) (Show >|< This), Type String] a@.
--
-- When @a@ satisfies @In as a@, you can use the method @'proj' :: 'AllOf' as f -> f a@.
--
-- Clusses call for some language extensions. Basically, this language pragma will do.
--
-- >{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
--
-- Internally, "type pattern matching" is executed by 'Where', a closed type family, which cannot check if a type satisfies a constraint.
-- If @as@ has many type patterns that can match @a@, only the first one matches @a@.
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
type AllOfI as = AllOfI' as as
type AllOfI' ts as = AllOf' ts as Id
andI :: a -> AllOfI' ts as -> AllOfI' ts (Type a ': as)
andI x y = And (Id x) y
andI1 :: (forall b. Modify (In ts) p b => a b) -> AllOfI' ts as -> AllOfI' ts (a <| p ': as)
andI1 x y = And1 (Id x) y
andI2 :: (forall b c. Modify2 (In ts) p b c => a b c) -> AllOfI' ts as -> AllOfI' ts (a <| p ': as)
andI2 x y = And2 (Id x) y
andI3 :: (forall b c d. Modify3 (In ts) p b c d => a b c d) -> AllOfI' ts as -> AllOfI' ts (a <| p ': as)
andI3 x y = And3 (Id x) y
andI4 :: (forall b c d e. Modify4 (In ts) p b c d e => a b c d e) -> AllOfI' ts as -> AllOfI' ts (a <| p ': as)
andI4 x y = And4 (Id x) y
andI5 :: (forall b c d e f. Modify5 (In ts) p b c d e f => a b c d e f) -> AllOfI' ts as -> AllOfI' ts (a <| p ': as)
andI5 x y = And5 (Id x) y
andI6 :: (forall b c d e f g. Modify6 (In ts) p b c d e f g => a b c d e f g) -> AllOfI' ts as -> AllOfI' ts (a <| p ': as)
andI6 x y = And6 (Id x) y
noneI :: AllOfI' ts '[]
noneI = None
projI :: In as a => AllOfI as -> a
projI = unId . proj

newtype Func b a = Func {unFunc :: a -> b}
type AllOfF as t = AllOfF' as as t
type AllOfF' ts as t = AllOf' ts as (Func t)
andF :: (a -> t) -> AllOfF' ts as t -> AllOfF' ts (Type a ': as) t
andF x y = And (Func x) y
andF1 :: (forall b. Modify (In ts) p b => a b -> t) -> AllOfF' ts as t -> AllOfF' ts (a <| p ': as) t
andF1 x y = And1 (Func x) y
andF2 :: (forall b c. Modify2 (In ts) p b c => a b c -> t) -> AllOfF' ts as t -> AllOfF' ts (a <| p ': as) t
andF2 x y = And2 (Func x) y
andF3 :: (forall b c d. Modify3 (In ts) p b c d => a b c d -> t) -> AllOfF' ts as t -> AllOfF' ts (a <| p ': as) t
andF3 x y = And3 (Func x) y
andF4 :: (forall b c d e. Modify4 (In ts) p b c d e => a b c d e -> t) -> AllOfF' ts as t -> AllOfF' ts (a <| p ': as) t
andF4 x y = And4 (Func x) y
andF5 :: (forall b c d e f. Modify5 (In ts) p b c d e f => a b c d e f -> t) -> AllOfF' ts as t -> AllOfF' ts (a <| p ': as) t
andF5 x y = And5 (Func x) y
andF6 :: (forall b c d e f g. Modify6 (In ts) p b c d e f g => a b c d e f g -> t) -> AllOfF' ts as t -> AllOfF' ts (a <| p ': as) t
andF6 x y = And6 (Func x) y
noneF :: AllOfF' ts '[] t
noneF = None
projF :: In as a => AllOfF as t -> (a -> t)
projF = unFunc . proj

infixr 0 `andI`, `andI1`, `andI2`, `andI3`, `andI4`, `andI5`, `andI6`, `andF`, `andF1`, `andF2`, `andF3`, `andF4`, `andF5`, `andF6`
