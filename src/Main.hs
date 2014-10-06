{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}
{-# LANGUAGE TemplateHaskell, ConstraintKinds #-}

module Main (module Main) where

import Type.Cluss
import Type.Cluss.TH
import Data.Monoid

type Hellos = [
    Type String,
    Type Int,
    Type Double,
    Unary [] Show,
    Quaternary (,,,) (This >|< This >||< This >|||< This)]
hello :: In Hellos a => a -> String
hello = projF (
    (\s -> "hello, " ++ s) `andF`
    (\n -> "Mr. " ++ show n) `andF`
    (\x -> show (x / 2) ++ " times two") `andF`
    (\xs -> concatMap ((++", ") . show) xs ++ "period") `andF1`
    (\(x,y,z,w) ->
        hello x ++ " and " ++ hello y ++ " and " ++
        hello z ++ " and " ++ hello w) `andF4`
    noneF :: AllOfF Hellos String)

hellomain :: IO ()
hellomain = putStrLn $
    hello ("world", 42 :: Int, 3.14 :: Double, [True, False])

type Printfs = [Binary (->) (Show >|< This), Type String]
printf :: In Printfs a => String -> a
printf s = projI (
    (\x -> printf (go s (show x))) `andI2`
    s `andI`
    noneI :: AllOfI Printfs)
  where
    go ('@' : cs) t = t ++ cs
    go (c : cs) t = c : go cs t
    go [] _ = error "there is no '@' any more!"

printfmain :: IO ()
printfmain = putStrLn $
    printf "@ good @ and @" 12 "men" True

type Monads = [AnyType (Is []), Unary (->) Pure, Unary (,) Monoid, Unary Wrap This]
newtype Wrap m a = Wrap {unWrap :: m a}
newtype Bind a b m = Bind {unBind :: m a -> (a -> m b) -> m b}
newtype Return a m = Return {unReturn :: a -> m a}
bind :: In Monads m => m a -> (a -> m b) -> m b
bind = unBind $ proj (
    Bind (\m k -> concatMap k m) `AndAny`
    Bind (\m k e -> k (m e) e) `And1`
    Bind (\(a,x) k -> let (a',x') = k x in (a<>a',x')) `And1`
    Bind (\m k -> Wrap (unWrap m `bind` (unWrap . k))) `And1`
    None :: AllOf Monads (Bind a b))
return' :: In Monads m => a -> m a
return' = unReturn $ proj (
    Return (: []) `AndAny`
    Return const `And1`
    Return ((,) mempty) `And1`
    Return (Wrap . return') `And1`
    None :: AllOf Monads (Return a))
infixl 1 `bind`

monadmain :: IO ()
monadmain = print $
    return' 1 `bind` replicate 3 `bind` (\n -> [n .. n+4])

type Num' = $(clussify ''Num)
type Show' = $(clussify ''Show)

main :: IO ()
main = monadmain
