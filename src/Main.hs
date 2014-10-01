{-# LANGUAGE DataKinds, FlexibleContexts, TypeOperators #-}

module Main(module Main) where

import Type.Cluss

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
    go [] t = error "there is no '@' any more!"

printfmain = putStrLn $
    printf "@ good @ and @" 12 "men" True

main = printfmain
