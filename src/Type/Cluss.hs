{-# LANGUAGE
    Trustworthy,
    ScopedTypeVariables, TypeOperators,
    RankNTypes, TypeFamilies,
    DataKinds, ConstraintKinds, PolyKinds,
    MultiParamTypeClasses, FlexibleInstances, UndecidableInstances, FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
-- Module: Type.Cluss
-- Copyright: (c) Yusuke Matsushita 2014
-- License: BSD3
-- Maintainer: Yusuke Matsushita
-- Stability: provisional
-- Portability: portable
--
-- Basic tools for clusses.
-- <#g:11 Examples> show how to use them.
--
-- Template haskell tools are in the module <Type-Cluss-TH.html Type.Cluss.TH>.
--------------------------------------------------------------------------------

module Type.Cluss (
    -- * Clusses
    In(..), Has

    -- * Type Patterns
    {-| Type patterns are used in the type list (first parameter) of 'In'.
        Each type pattern corresponds to the head of an instance declaration for a type class, namely, @instance ... where@. -}
  , Type, AnyType, type (<|), Unary, Binary, Ternary, Quaternary, Quinary, Senary, Septenary, Octary, Nonary, Denary

    -- * Instance Products
  , AllOf, AllOf'(..)

    -- * Constraint Combinators
    {-| Constraint combinators are used in the second parameter of <|, 'Unary', 'Binary', ..., or 'Denary'.
        Note that each combinator is kind-polymorphic. -}

    -- ** Basic Combinators
  , This, Pure, Is

    -- ** Combinators for Overlaying Contraints
  , type (>+<), type (>++<), type (>+++<), type (>++++<), type (>+++++<), type (>++++++<), type (>+++++++<), type (>++++++++<), type (>+++++++++<), type (>++++++++++<)

    -- ** Combinators for Bonding Contraints
  , type (>|<), type (>||<), type (>|||<), type (>||||<), type (>|||||<), type (>||||||<), type (>|||||||<), type (>||||||||<), type (>|||||||||<)

    -- * Helpers

    -- ** Helpers for Identical Constructors
  , AllOfI, andI, andI1, andI2, andI3, andI4, andI5, andI6, andI7, andI8, andI9, andI10, noneI, projI

    -- ** Helpers for Function Constructors
  , AllOfF, andF, andF1, andF2, andF3, andF4, andF5, andF6, andF7, andF8, andF9, andF10, noneF, projF

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
-- >    go [] _ = error "there is no '@' any more!"
-- >
-- >main = putStrLn $
-- >    printf "@ good @ and @" 12 "men" True
--
-- This is the result.
--
-- >12 good "men" and True

-- $monad
-- Here is a more complex example.
-- When the type of a \"cluss method\" is complex, you generally have to create newtypes (like Bind and Return below).
--
-- >type Monads = [Type [], Unary (->) Pure, Unary (,) Monoid, Unary Wrap This]
-- >newtype Wrap m a = Wrap {unWrap :: m a}
-- >newtype Bind a b m = Bind {unBind :: m a -> (a -> m b) -> m b}
-- >newtype Return a m = Return {unReturn :: a -> m a}
-- >bind :: In Monads m => m a -> (a -> m b) -> m b
-- >bind = unBind $ proj (
-- >    Bind (\m k -> concatMap k m) `And`
-- >    Bind (\m k e -> k (m e) e) `And1`
-- >    Bind (\(a,x) k -> let (a2,x2) = k x in (a<>a2,x2)) `And1`
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

type a $ b = a b
infixr 0 $

type family (a :: Bool) && (b :: Bool) :: Bool
type instance True && True = True
type instance True && False = False
type instance False && True = False
type instance False && False = False
type family If (a :: Bool) (b :: *) (c :: *) :: *
type instance If True b c = b
type instance If False b c = c

data Look_At_Head
data Look_At_Tail a
data No_I_Don't_Have_That

-- | The empty type @'Type' a@ is a type pattern,
-- where @a@ is a type.
-- The type pattern @'Type' 'Int'@
-- corresponds to @instance C 'Int' where ...@ (where @C@ is a corresponding type class), for example.
-- Note that the type variable @a@ can be of any kind: @a@ could be of the kind @* -> *@, for example.
data Type (a :: k)
-- | The empty type @'AnyType' p@ is a type pattern,
-- where @p@ is a type function from a type to a constraint.
-- The type pattern @'AnyType' 'Show'@
-- basically corresponds to @instance 'Show' a => C a where ...@ (where @C@ is a corresponding type class), for example,
-- but 'AnyType' is much more useful
-- in that it does not cause /overlapping instances/ whereas 'C' is likely to,
-- because cluss instances are prioritized.
data AnyType (p :: k -> Constraint)
-- | The empty type @a '<|' p@ is a type pattern,
-- where @a@ is a type constructor,
-- and @p@ is a type function to a constraint from the type variables for the constructor @a@.
-- The type pattern @[] <| 'Show'@
-- corresponds to @instance 'Show' a => C [a] where ...@ (where @C@ is a corresponding type class), for example.
--
-- You can replace 'Unary', 'Binary', ..., and 'Denary' with '<|',
-- but you can sometimes save the effort of annotating kinds
-- using 'Unary', 'Binary', ..., or 'Denary' instead of '<|',
-- especially when using the @PolyKinds@ extension,
-- because the kinds of the parameters in 'Unary', 'Binary', ..., and 'Denary' are restricted as described below.
data (a :: k) <| (p :: l)
-- | @a <| p@, with @a@ being of the kind @i -> k@, and @p@ of the kind @i -> 'Constraint'@.
type Unary (a :: i -> k) (p :: i -> Constraint) = a <| p
-- | @a <| p@, with @a@ being of the kind @i -> i2 -> k@, and @p@ of the kind @i -> i2 -> 'Constraint'@.
type Binary (a :: i -> i2 -> k) (p :: i -> i2 -> Constraint) = a <| p
-- | @a <| p@, with @a@ being of the kind @i -> i2 -> i3 -> k@, and @p@ of the kind @i -> i2 -> i3 -> 'Constraint'@.
type Ternary (a :: i -> i2 -> i3 -> k) (p :: i -> i2 -> i3 -> Constraint) = a <| p
-- | @a <| p@, with @a@ being of the kind @i -> i2 -> i3 -> i4 -> k@, and @p@ of the kind @i -> i2 -> i3 -> i4 -> 'Constraint'@.
type Quaternary (a :: i -> i2 -> i3 -> i4 -> k) (p :: i -> i2 -> i3 -> i4 -> Constraint) = a <| p
-- | @a <| p@, with @a@ being of the kind @i -> i2 -> i3 -> i4 -> i5 -> k@, and @p@ of the kind @i -> i2 -> i3 -> i4 -> i5 -> 'Constraint'@.
type Quinary (a :: i -> i2 -> i3 -> i4 -> i5 -> k) (p :: i -> i2 -> i3 -> i4 -> i5 -> Constraint) = a <| p
-- | @a <| p@, with @a@ being of the kind @i -> i2 -> i3 -> i4 -> i5 -> i6 -> k@, and @p@ of the kind @i -> i2 -> i3 -> i4 -> i5 -> i6 -> 'Constraint'@.
type Senary (a :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> k) (p :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> Constraint) = a <| p
-- | @a <| p@, with @a@ being of the kind @i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> k@, and @p@ of the kind @i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> 'Constraint'@.
type Septenary (a :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> k) (p :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> Constraint) = a <| p
-- | @a <| p@, with @a@ being of the kind @i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> k@, and @p@ of the kind @i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> 'Constraint'@.
type Octary (a :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> k) (p :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> Constraint) = a <| p
-- | @a <| p@, with @a@ being of the kind @i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> i9 -> k@, and @p@ of the kind @i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> i9 -> 'Constraint'@.
type Nonary (a :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> i9 -> k) (p :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> i9 -> Constraint) = a <| p
-- | @a <| p@, with @a@ being of the kind @i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> i9 -> i10 -> k@, and @p@ of the kind @i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> i9 -> i10 -> 'Constraint'@.
type Denary (a :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> i9 -> i10 -> k) (p :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> i9 -> i10 -> Constraint) = a <| p

-- | 'This' creates a recursion.
-- In other words, 'This' will work as @'In' as@ itself
-- when used in the type list (first parameter) @as@ of 'In',
-- combined with 'Type', '<|', 'Unary', 'Binary', ..., 'Denary',
-- \>+\<, \>++\<, ..., \>++++++++++\<,
-- \>|\<, \>||\<, ..., and \>|||||||||\<.
--
-- Note that 'This' will not be expanded into @'In' as@
-- if the condition described above is not satisfied.
-- For example, 'This' in the parameter of `AnyType' will not be expanded
-- because it causes infinite recursion.
-- Internally, the expansion is executed by Modify, Modify2, ..., and Modify10.
--
-- The instance of 'This' itself cannot be created
-- since the context @'True' ~ 'False'@ will never be satisfied.
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
-- >(p >+++++++< q) a b c d e f g == (p a b c d e f g, q a b c d e f g)
class (p a b c d e f g, q a b c d e f g) => (>+++++++<) p q a b c d e f g
instance (p a b c d e f g, q a b c d e f g) => (>+++++++<) p q a b c d e f g
-- |
-- >(p >++++++++< q) a b c d e f g h == (p a b c d e f g h, q a b c d e f g h)
class (p a b c d e f g h, q a b c d e f g h) => (>++++++++<) p q a b c d e f g h
instance (p a b c d e f g h, q a b c d e f g h) => (>++++++++<) p q a b c d e f g h
-- |
-- >(p >+++++++++< q) a b c d e f g h i == (p a b c d e f g h, q a b c d e f g h i)
class (p a b c d e f g h i, q a b c d e f g h i) => (>+++++++++<) p q a b c d e f g h i
instance (p a b c d e f g h i, q a b c d e f g h i) => (>+++++++++<) p q a b c d e f g h i
-- |
-- >(p >++++++++++< q) a b c d e f g h i j == (p a b c d e f g h, q a b c d e f g h i j)
class (p a b c d e f g h i j, q a b c d e f g h i j) => (>++++++++++<) p q a b c d e f g h i j
instance (p a b c d e f g h i j, q a b c d e f g h i j) => (>++++++++++<) p q a b c d e f g h i j
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
-- >(p >||||||< q) a b c d e f g == (p a b c d e f, q g)
class (p a b c d e f, q g) => (>||||||<) p q a b c d e f g
instance (p a b c d e f, q g) => (>||||||<) p q a b c d e f g
-- |
-- >(p >|||||||< q) a b c d e f g h == (p a b c d e f g, q h)
class (p a b c d e f g, q h) => (>|||||||<) p q a b c d e f g h
instance (p a b c d e f g, q h) => (>|||||||<) p q a b c d e f g h
-- |
-- >(p >||||||||< q) a b c d e f g h i == (p a b c d e f g h, q i)
class (p a b c d e f g h, q i) => (>||||||||<) p q a b c d e f g h i
instance (p a b c d e f g h, q i) => (>||||||||<) p q a b c d e f g h i
-- |
-- >(p >||||||||< q) a b c d e f g h i j == (p a b c d e f g h i, q j)
class (p a b c d e f g h i, q j) => (>|||||||||<) p q a b c d e f g h i j
instance (p a b c d e f g h i, q j) => (>|||||||||<) p q a b c d e f g h i j

-- |
-- >(Is a) b == (a ~ b)
type Is = (~)

infixl 7 <|
infixl 8 >|<, >||<, >|||<, >||||<, >|||||<, >||||||<, >|||||||<, >||||||||<, >|||||||||<
infixl 9 >+<, >++<, >+++<, >++++<, >+++++<, >++++++<, >+++++++<, >++++++++<, >+++++++++<, >++++++++++<

type family Where (ts :: [*]) (as :: [*]) (a :: k) :: * where
    Where ts (Type a ': as) a = Look_At_Head
    Where ts (AnyType p ': as) a = If (Check p a) Look_At_Head $ Look_At_Tail $ Where ts as $ a
    Where ts (Unary a p ': as) (a b) = If (Check (Modify a (In ts) p) b) Look_At_Head $ Look_At_Tail $ Where ts as $ a b
    Where ts (Binary a p ': as) (a b c) = If (Check2 (Modify2 a (In ts) p) b c) Look_At_Head $ Look_At_Tail $ Where ts as $ a b c
    Where ts (Ternary a p ': as) (a b c d) = If (Check3 (Modify3 a (In ts) p) b c d) Look_At_Head $ Look_At_Tail $ Where ts as $ a b c d
    Where ts (Quaternary a p ': as) (a b c d e) = If (Check4 (Modify4 a (In ts) p) b c d e) Look_At_Head $ Look_At_Tail $ Where ts as $ a b c d e
    Where ts (Quinary a p ': as) (a b c d e f) = If (Check5 (Modify5 a (In ts) p) b c d e f) Look_At_Head $ Look_At_Tail $ Where ts as $ a b c d e f
    Where ts (Senary a p ': as) (a b c d e f g) = If (Check6 (Modify6 a (In ts) p) b c d e f g) Look_At_Head $ Look_At_Tail $ Where ts as $ a b c d e f g
    Where ts (Septenary a p ': as) (a b c d e f g h) = If (Check7 (Modify7 a (In ts) p) b c d e f g h) Look_At_Head $ Look_At_Tail $ Where ts as $ a b c d e f g h
    Where ts (Octary a p ': as) (a b c d e f g h i) = If (Check8 (Modify8 a (In ts) p) b c d e f g h i) Look_At_Head $ Look_At_Tail $ Where ts as $ a b c d e f g h i
    Where ts (Nonary a p ': as) (a b c d e f g h i j) = If (Check9 (Modify9 a (In ts) p) b c d e f g h i j) Look_At_Head $ Look_At_Tail $ Where ts as $ a b c d e f g h i j
    Where ts (Denary a p ': as) (a b c d e f g h i j k) = If (Check10 (Modify10 a (In ts) p) b c d e f g h i j k) Look_At_Head $ Look_At_Tail $ Where ts as $ a b c d e f g h i j k
    Where ts (b ': as) a = Look_At_Tail (Where ts as a)
    Where ts '[] a = No_I_Don't_Have_That

-- | @'Has' as a@ judges whether a type @a@ belongs to a cluss @'In' as@, on some level.
-- When not sure, 'Has' always returns 'True'.
-- For example, when @as@ has @'Unary' [] 'Show'@ and @a@ is @[b]@,
-- 'Has' can't judge if @b@ belongs to 'Show' since the instances of 'Show' is /open/,
-- but it assumes that @b@ belongs to 'Show' and
-- returns 'True'.
type family Has (as :: [*]) (a :: k) :: Bool
type instance Has as a = Has' (Where as as a)
type family Has' (n :: *) :: Bool
type instance Has' Look_At_Head = True
type instance Has' (Look_At_Tail n) = Has' n
type instance Has' No_I_Don't_Have_That = False

type family Modify (a :: k) (this :: k -> Constraint) (p :: i -> Constraint) :: i -> Constraint where
    Modify a this This = this
    Modify a this (p >+< q) = Modify a this p >+< Modify a this q
    Modify a this p = p
type family Modify2 (a :: k) (this :: k -> Constraint) (p :: i -> i2 -> Constraint) :: i -> i2 -> Constraint where
    Modify2 a this (p >++< q) = Modify2 a this p >++< Modify2 a this q
    Modify2 a this (p >|< q) = Modify a this p >|< Modify a this q
    Modify2 a this p = p
type family Modify3 (a :: k) (this :: k -> Constraint) (p :: i -> i2 -> i3 -> Constraint) :: i -> i2 -> i3 -> Constraint where
    Modify3 a this (p >+++< q) = Modify3 a this p >+++< Modify3 a this q
    Modify3 a this (p >||< q) = Modify2 a this p >||< Modify a this q
    Modify3 a this p = p
type family Modify4 (a :: k) (this :: k -> Constraint) (p :: i -> i2 -> i3 -> i4 -> Constraint) :: i -> i2 -> i3 -> i4 -> Constraint where
    Modify4 a this (p >++++< q) = Modify4 a this p >++++< Modify4 a this q
    Modify4 a this (p >|||< q) = Modify3 a this p >|||< Modify a this q
    Modify4 a this p = p
type family Modify5 (a :: k) (this :: k -> Constraint) (p :: i -> i2 -> i3 -> i4 -> i5 -> Constraint) :: i -> i2 -> i3 -> i4 -> i5 -> Constraint where
    Modify5 a this (p >+++++< q) = Modify5 a this p >+++++< Modify5 a this q
    Modify5 a this (p >||||< q) = Modify4 a this p >||||< Modify a this q
    Modify5 a this p = p
type family Modify6 (a :: k) (this :: k -> Constraint) (p :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> Constraint) :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> Constraint where
    Modify6 a this (p >++++++< q) = Modify6 a this p >++++++< Modify6 a this q
    Modify6 a this (p >|||||< q) = Modify5 a this p >|||||< Modify a this q
    Modify6 a this p = p
type family Modify7 (a :: k) (this :: k -> Constraint) (p :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> Constraint) :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> Constraint where
    Modify7 a this (p >+++++++< q) = Modify7 a this p >+++++++< Modify7 a this q
    Modify7 a this (p >||||||< q) = Modify6 a this p >||||||< Modify a this q
    Modify7 a this p = p
type family Modify8 (a :: k) (this :: k -> Constraint) (p :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> Constraint) :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> Constraint where
    Modify8 a this (p >++++++++< q) = Modify8 a this p >++++++++< Modify8 a this q
    Modify8 a this (p >|||||||< q) = Modify7 a this p >|||||||< Modify a this q
    Modify8 a this p = p
type family Modify9 (a :: k) (this :: k -> Constraint) (p :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> i9 -> Constraint) :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> i9 -> Constraint where
    Modify9 a this (p >+++++++++< q) = Modify9 a this p >+++++++++< Modify9 a this q
    Modify9 a this (p >||||||||< q) = Modify8 a this p >||||||||< Modify a this q
    Modify9 a this p = p
type family Modify10 (a :: k) (this :: k -> Constraint) (p :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> i9 -> i10 -> Constraint) :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> i9 -> i10 -> Constraint where
    Modify10 a this (p >++++++++++< q) = Modify10 a this p >++++++++++< Modify10 a this q
    Modify10 a this (p >|||||||||< q) = Modify9 a this p >|||||||||< Modify a this q
    Modify10 a this p = p

type family Check (p :: i -> Constraint) (b :: i) :: Bool where
    Check (In as) b = Has as b
    Check (p >+< q) b = Check p b && Check q b
    Check (Is b) b = True
    Check (Is b') b = False
    Check p b = True
type family Check2 (p :: i -> i2 -> Constraint) (b :: i) (c :: i2) :: Bool where
    Check2 (p >++< q) b c = Check2 p b c && Check2 q b c
    Check2 (p >|< q) b c = Check p b && Check q c
    Check2 p b c = True
type family Check3 (p :: i -> i2 -> i3 -> Constraint) (b :: i) (c :: i2) (d :: i3) :: Bool where
    Check3 (p >+++< q) b c d = Check3 p b c d && Check3 q b c d
    Check3 (p >||< q) b c d = Check2 p b c && Check q d
    Check3 p b c d = True
type family Check4 (p :: i -> i2 -> i3 -> i4 -> Constraint) (b :: i) (c :: i2) (d :: i3) (e :: i4) :: Bool where
    Check4 (p >++++< q) b c d e = Check4 p b c d e && Check4 q b c d e
    Check4 (p >|||< q) b c d e = Check3 p b c d && Check q e
    Check4 p b c d e = True
type family Check5 (p :: i -> i2 -> i3 -> i4 -> i5 -> Constraint) (b :: i) (c :: i2) (d :: i3) (e :: i4) (f :: i5) :: Bool where
    Check5 (p >+++++< q) b c d e f = Check5 p b c d e f && Check5 q b c d e f
    Check5 (p >||||< q) b c d e f = Check4 p b c d e && Check q f
    Check5 p b c d e f = True
type family Check6 (p :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> Constraint) (b :: i) (c :: i2) (d :: i3) (e :: i4) (f :: i5) (g :: i6) :: Bool where
    Check6 (p >++++++< q) b c d e f g = Check6 p b c d e f g && Check6 q b c d e f g
    Check6 (p >|||||< q) b c d e f g = Check5 p b c d e f && Check q g
    Check6 p b c d e f g = True
type family Check7 (p :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> Constraint) (b :: i) (c :: i2) (d :: i3) (e :: i4) (f :: i5) (g :: i6) (h :: i7) :: Bool where
    Check7 (p >+++++++< q) b c d e f g h = Check7 p b c d e f g h && Check7 q b c d e f g h
    Check7 (p >||||||< q) b c d e f g h = Check6 p b c d e f g && Check q h
    Check7 p b c d e f g h = True
type family Check8 (p :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> Constraint) (b :: i) (c :: i2) (d :: i3) (e :: i4) (f :: i5) (g :: i6) (h :: i7) (i' :: i8) :: Bool where
    Check8 (p >++++++++< q) b c d e f g h i = Check8 p b c d e f g h i && Check8 q b c d e f g h i
    Check8 (p >|||||||< q) b c d e f g h i = Check7 p b c d e f g h && Check q i
    Check8 p b c d e f g h i = True
type family Check9 (p :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> i9 -> Constraint) (b :: i) (c :: i2) (d :: i3) (e :: i4) (f :: i5) (g :: i6) (h :: i7) (i' :: i8) (j :: i9) :: Bool where
    Check9 (p >+++++++++< q) b c d e f g h i j = Check9 p b c d e f g h i j && Check9 q b c d e f g h i j
    Check9 (p >||||||||< q) b c d e f g h i j = Check8 p b c d e f g h i && Check q j
    Check9 p b c d e f g h i j = True
type family Check10 (p :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> i9 -> i10 -> Constraint) (b :: i) (c :: i2) (d :: i3) (e :: i4) (f :: i5) (g :: i6) (h :: i7) (i' :: i8) (j :: i9) (k' :: i10) :: Bool where
    Check10 (p >++++++++++< q) b c d e f g h i j k = Check10 p b c d e f g h i j k && Check10 q b c d e f g h i j k
    Check10 (p >|||||||||< q) b c d e f g h i j k = Check9 p b c d e f g h i j && Check q k
    Check10 p b c d e f g h i j k = True

-- | @'AllOf' as f@ is a tuple that contains values of the type @f a@,
-- where @a@ can be any type that satisfies @In as a@.
-- Each value corresponds to each type pattern,
-- and the values in @'AllOf' as f@ must be in the same order as the type patterns in @as@.
-- And, And1, And2, ..., and And10 are used to combine the values,
-- where None must be added at the end.
-- You have to use And for @'Type' a@,
-- And1 for @'Unary' a p@, And2 for  @'Binary' a p@,
-- ..., and And10 for @'Denary' a p@.
type AllOf as = AllOf' as as
data family AllOf' (ts :: [*]) (as :: [*]) (t :: k -> *)
data instance AllOf' ts (Type a ': as) t = And (t a) (AllOf' ts as t)
data instance AllOf' ts (AnyType p ': as) t = AndAny (forall a. p a => t a) (AllOf' ts as t)
data instance AllOf' ts (Unary a p ': as) t = And1 (forall b. Modify (a b) (In ts) p b => t (a b)) (AllOf' ts as t)
data instance AllOf' ts (Binary a p ': as) t = And2 (forall b c. Modify2 (a b c) (In ts) p b c => t (a b c)) (AllOf' ts as t )
data instance AllOf' ts (Ternary a p ': as) t = And3 (forall b c d. Modify3 (a b c d) (In ts) p b c d => t (a b c d)) (AllOf' ts as t)
data instance AllOf' ts (Quaternary a p ': as) t = And4 (forall b c d e. Modify4 (a b c d e) (In ts) p b c d e => t (a b c d e)) (AllOf' ts as t)
data instance AllOf' ts (Quinary a p ': as) t = And5 (forall b c d e f. Modify5 (a b c d e f) (In ts) p b c d e f => t (a b c d e f)) (AllOf' ts as t)
data instance AllOf' ts (Senary a p ': as) t = And6 (forall b c d e f g. Modify6 (a b c d e f g) (In ts) p b c d e f g => t (a b c d e f g)) (AllOf' ts as t)
data instance AllOf' ts (Septenary a p ': as) t = And7 (forall b c d e f g h. Modify7 (a b c d e f g h) (In ts) p b c d e f g h => t (a b c d e f g h)) (AllOf' ts as t)
data instance AllOf' ts (Octary a p ': as) t = And8 (forall b c d e f g h i. Modify8 (a b c d e f g h i) (In ts) p b c d e f g h i => t (a b c d e f g h i)) (AllOf' ts as t)
data instance AllOf' ts (Nonary a p ': as) t = And9 (forall b c d e f g h i j. Modify9 (a b c d e f g h i j) (In ts) p b c d e f g h i j => t (a b c d e f g h i j)) (AllOf' ts as t)
data instance AllOf' ts (Denary a p ': as) t = And10 (forall b c d e f g h i j k. Modify10 (a b c d e f g h i j k) (In ts) p b c d e f g h i j k => t (a b c d e f g h i j k)) (AllOf' ts as t)
data instance AllOf' ts '[] t = None

infixr 0 `And`, `AndAny`, `And1`, `And2`, `And3`, `And4`, `And5`, `And6`, `And7`, `And8`, `And9`, `And10`

-- | @'In' as@ is a /cluss/, where @as@ is a list of type patterns.
-- Normally, @as@ is concrete and does not contain any type variables, like @'In' ['Binary' (->) ('Show' >|< 'This'), 'Type' 'String'] a@.
--
-- When @a@ satisfies @In as a@, you can use the method @'proj' :: 'AllOf' as f -> f a@.
--
--
-- Internally, \"type pattern matching\" is executed by Where, a closed type family, which cannot check if a type satisfies a constraint.
-- If @as@ has many type patterns that can match @a@, only the first one matches @a@.
--
-- Clusses call for some language extensions. Basically, this language pragma will do.
--
-- >{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
class In (as :: [*]) (a :: k) where
    proj :: AllOf as t -> t a
instance In' (Where as as a) as as a => In as a where
    proj = proj' (undefined :: Where as as a)
class In' (n :: *) (ts :: [*]) (as :: [*]) (a :: k) where
    proj' :: n -> AllOf' ts as t -> t a
instance In' Look_At_Head ts (Type a ': as) a where
    proj' _ (And x _) = x
instance p a => In' Look_At_Head ts (AnyType p ': as) a where
    proj' _ (AndAny x _) = x
instance Modify (a b) (In ts) p b => In' Look_At_Head ts (Unary a p ': as) (a b) where
    proj' _ (And1 x _) = x
instance Modify2 (a b c) (In ts) p b c => In' Look_At_Head ts (Binary a p ': as) (a b c) where
    proj' _ (And2 x _) = x
instance Modify3 (a b c d) (In ts) p b c d => In' Look_At_Head ts (Ternary a p ': as) (a b c d) where
    proj' _ (And3 x _) = x
instance Modify4 (a b c d e) (In ts) p b c d e => In' Look_At_Head ts (Quaternary a p ': as) (a b c d e) where
    proj' _ (And4 x _) = x
instance Modify5 (a b c d e f) (In ts) p b c d e f => In' Look_At_Head ts (Quinary a p ': as) (a b c d e f) where
    proj' _ (And5 x _) = x
instance Modify6 (a b c d e f g) (In ts) p b c d e f g => In' Look_At_Head ts (Senary a p ': as) (a b c d e f g) where
    proj' _ (And6 x _) = x
instance Modify7 (a b c d e f g h) (In ts) p b c d e f g h => In' Look_At_Head ts (Septenary a p ': as) (a b c d e f g h) where
    proj' _ (And7 x _) = x
instance Modify8 (a b c d e f g h i) (In ts) p b c d e f g h i => In' Look_At_Head ts (Octary a p ': as) (a b c d e f g h i) where
    proj' _ (And8 x _) = x
instance Modify9 (a b c d e f g h i j) (In ts) p b c d e f g h i j => In' Look_At_Head ts (Nonary a p ': as) (a b c d e f g h i j) where
    proj' _ (And9 x _) = x
instance Modify10 (a b c d e f g h i j k) (In ts) p b c d e f g h i j k => In' Look_At_Head ts (Denary a p ': as) (a b c d e f g h i j k) where
    proj' _ (And10 x _) = x
instance In' n ts as a => In' (Look_At_Tail n) ts (Type (b :: k) ': as) (a :: k) where
    proj' _ (And _ xs) = proj' (undefined :: n) xs
instance In' n ts as a => In' (Look_At_Tail n) ts (AnyType (p :: k -> Constraint) ': as) (a :: k) where
    proj' _ (AndAny _ xs) = proj' (undefined :: n) xs
instance In' n ts as a => In' (Look_At_Tail n) ts (Unary (b :: i -> k) (p :: i -> Constraint) ': as) (a :: k) where
    proj' _ (And1 _ xs) = proj' (undefined :: n) xs
instance In' n ts as a => In' (Look_At_Tail n) ts (Binary (b :: i -> i2 -> k) (p :: i -> i2 -> Constraint) ': as) (a :: k) where
    proj' _ (And2 _ xs) = proj' (undefined :: n) xs
instance In' n ts as a => In' (Look_At_Tail n) ts (Ternary (b :: i -> i2 -> i3 -> k) (p :: i -> i2 -> i3 -> Constraint) ': as) (a :: k) where
    proj' _ (And3 _ xs) = proj' (undefined :: n) xs
instance In' n ts as a => In' (Look_At_Tail n) ts (Quaternary (b :: i -> i2 -> i3 -> i4 -> k) (p :: i -> i2 -> i3 -> i4 -> Constraint) ': as) (a :: k) where
    proj' _ (And4 _ xs) = proj' (undefined :: n) xs
instance In' n ts as a => In' (Look_At_Tail n) ts (Quinary (b :: i -> i2 -> i3 -> i4 -> i5 -> k) (p :: i -> i2 -> i3 -> i4 -> i5 -> Constraint) ': as) (a :: k) where
    proj' _ (And5 _ xs) = proj' (undefined :: n) xs
instance In' n ts as a => In' (Look_At_Tail n) ts (Senary (b :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> k) (p :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> Constraint) ': as) (a :: k) where
    proj' _ (And6 _ xs) = proj' (undefined :: n) xs
instance In' n ts as a => In' (Look_At_Tail n) ts (Septenary (b :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> k) (p :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> Constraint) ': as) (a :: k) where
    proj' _ (And7 _ xs) = proj' (undefined :: n) xs
instance In' n ts as a => In' (Look_At_Tail n) ts (Octary (b :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> k) (p :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> Constraint) ': as) (a :: k) where
    proj' _ (And8 _ xs) = proj' (undefined :: n) xs
instance In' n ts as a => In' (Look_At_Tail n) ts (Nonary (b :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> i9 -> k) (p :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> i9 -> Constraint) ': as) (a :: k) where
    proj' _ (And9 _ xs) = proj' (undefined :: n) xs
instance In' n ts as a => In' (Look_At_Tail n) ts (Denary (b :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> i9 -> i10 -> k) (p :: i -> i2 -> i3 -> i4 -> i5 -> i6 -> i7 -> i8 -> i9 -> i10 -> Constraint) ': as) (a :: k) where
    proj' _ (And10 _ xs) = proj' (undefined :: n) xs

newtype Id a = Id {unId :: a}
type AllOfI as = AllOfI' as as
type AllOfI' ts as = AllOf' ts as Id
andI :: a -> AllOfI' ts as -> AllOfI' ts (Type a ': as)
andI x y = And (Id x) y
andI1 :: (forall b. Modify (a b) (In ts) p b => a b) -> AllOfI' ts as -> AllOfI' ts (Unary a p ': as)
andI1 x y = And1 (Id x) y
andI2 :: (forall b c. Modify2 (a b c) (In ts) p b c => a b c) -> AllOfI' ts as -> AllOfI' ts (Binary a p ': as)
andI2 x y = And2 (Id x) y
andI3 :: (forall b c d. Modify3 (a b c d) (In ts) p b c d => a b c d) -> AllOfI' ts as -> AllOfI' ts (Ternary a p ': as)
andI3 x y = And3 (Id x) y
andI4 :: (forall b c d e. Modify4 (a b c d e) (In ts) p b c d e => a b c d e) -> AllOfI' ts as -> AllOfI' ts (Quaternary a p ': as)
andI4 x y = And4 (Id x) y
andI5 :: (forall b c d e f. Modify5 (a b c d e f) (In ts) p b c d e f => a b c d e f) -> AllOfI' ts as -> AllOfI' ts (Quinary a p ': as)
andI5 x y = And5 (Id x) y
andI6 :: (forall b c d e f g. Modify6 (a b c d e f g) (In ts) p b c d e f g => a b c d e f g) -> AllOfI' ts as -> AllOfI' ts (Senary a p ': as)
andI6 x y = And6 (Id x) y
andI7 :: (forall b c d e f g h. Modify7 (a b c d e f g h) (In ts) p b c d e f g h => a b c d e f g h) -> AllOfI' ts as -> AllOfI' ts (Septenary a p ': as)
andI7 x y = And7 (Id x) y
andI8 :: (forall b c d e f g h i. Modify8 (a b c d e f g h i) (In ts) p b c d e f g h i => a b c d e f g h i) -> AllOfI' ts as -> AllOfI' ts (Octary a p ': as)
andI8 x y = And8 (Id x) y
andI9 :: (forall b c d e f g h i j. Modify9 (a b c d e f g h i j) (In ts) p b c d e f g h i j => a b c d e f g h i j) -> AllOfI' ts as -> AllOfI' ts (Nonary a p ': as)
andI9 x y = And9 (Id x) y
andI10 :: (forall b c d e f g h i j k. Modify10 (a b c d e f g h i j k) (In ts) p b c d e f g h i j k => a b c d e f g h i j k) -> AllOfI' ts as -> AllOfI' ts (Denary a p ': as)
andI10 x y = And10 (Id x) y
noneI :: AllOfI' ts '[]
noneI = None
projI :: In as a => AllOfI as -> a
projI = unId . proj

newtype Func b a = Func {unFunc :: a -> b}
type AllOfF as t = AllOfF' as as t
type AllOfF' ts as t = AllOf' ts as (Func t)
andF :: (a -> t) -> AllOfF' ts as t -> AllOfF' ts (Type a ': as) t
andF x y = And (Func x) y
andF1 :: (forall b. Modify (a b) (In ts) p b => a b -> t) -> AllOfF' ts as t -> AllOfF' ts (Unary a p ': as) t
andF1 x y = And1 (Func x) y
andF2 :: (forall b c. Modify2 (a b c) (In ts) p b c => a b c -> t) -> AllOfF' ts as t -> AllOfF' ts (Binary a p ': as) t
andF2 x y = And2 (Func x) y
andF3 :: (forall b c d. Modify3 (a b c d) (In ts) p b c d => a b c d -> t) -> AllOfF' ts as t -> AllOfF' ts (Ternary a p ': as) t
andF3 x y = And3 (Func x) y
andF4 :: (forall b c d e. Modify4 (a b c d e) (In ts) p b c d e => a b c d e -> t) -> AllOfF' ts as t -> AllOfF' ts (Quaternary a p ': as) t
andF4 x y = And4 (Func x) y
andF5 :: (forall b c d e f. Modify5 (a b c d e f) (In ts) p b c d e f => a b c d e f -> t) -> AllOfF' ts as t -> AllOfF' ts (Quinary a p ': as) t
andF5 x y = And5 (Func x) y
andF6 :: (forall b c d e f g. Modify6 (a b c d e f g) (In ts) p b c d e f g => a b c d e f g -> t) -> AllOfF' ts as t -> AllOfF' ts (Senary a p ': as) t
andF6 x y = And6 (Func x) y
andF7 :: (forall b c d e f g h. Modify7 (a b c d e f g h) (In ts) p b c d e f g h => a b c d e f g h -> t) -> AllOfF' ts as t -> AllOfF' ts (Septenary a p ': as) t
andF7 x y = And7 (Func x) y
andF8 :: (forall b c d e f g h i. Modify8 (a b c d e f g h i) (In ts) p b c d e f g h i => a b c d e f g h i -> t) -> AllOfF' ts as t -> AllOfF' ts (Octary a p ': as) t
andF8 x y = And8 (Func x) y
andF9 :: (forall b c d e f g h i j. Modify9 (a b c d e f g h i j) (In ts) p b c d e f g h i j => a b c d e f g h i j -> t) -> AllOfF' ts as t -> AllOfF' ts (Nonary a p ': as) t
andF9 x y = And9 (Func x) y
andF10 :: (forall b c d e f g h i j k. Modify10 (a b c d e f g h i j k) (In ts) p b c d e f g h i j k => a b c d e f g h i j k -> t) -> AllOfF' ts as t -> AllOfF' ts (Denary a p ': as) t
andF10 x y = And10 (Func x) y
noneF :: AllOfF' ts '[] t
noneF = None
projF :: In as a => AllOfF as t -> (a -> t)
projF = unFunc . proj

infixr 0 `andI`, `andI1`, `andI2`, `andI3`, `andI4`, `andI5`, `andI6`, `andI7`, `andI8`, `andI9`, `andI10`
infixr 0 `andF`, `andF1`, `andF2`, `andF3`, `andF4`, `andF5`, `andF6`, `andF7`, `andF8`, `andF9`, `andF10`
