{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
-- Module: Type.Cluss.TH
-- Copyright: (c) Yusuke Matsushita 2014
-- License: BSD3
-- Maintainer: Yusuke Matsushita
-- Stability: provisional
-- Portability: portable
--
-- Template haskell tools for clusses.
--
-- Basic tools are in the module <Type-Cluss.html Type.Cluss>.
--------------------------------------------------------------------------------

module Type.Cluss.TH (clussify) where

import Data.List
import Data.Maybe
import Language.Haskell.TH
import qualified Type.Cluss as C

appt :: Type -> Type -> Type
appt = AppT
infixl 1 `appt`

nary, ovrlp, bond :: Int -> Maybe Type
nary n = fmap ConT $ case n of
    1 -> return ''C.Unary
    2 -> return ''C.Binary
    3 -> return ''C.Ternary
    4 -> return ''C.Quaternary
    5 -> return ''C.Quinary
    6 -> return ''C.Senary
    7 -> return ''C.Septenary
    8 -> return ''C.Octary
    9 -> return ''C.Nonary
    10 -> return ''C.Denary
    _ -> Nothing
ovrlp n = fmap ConT $ case n of
    1 -> return ''(C.>+<)
    2 -> return ''(C.>++<)
    3 -> return ''(C.>+++<)
    4 -> return ''(C.>++++<)
    5 -> return ''(C.>+++++<)
    6 -> return ''(C.>++++++<)
    7 -> return ''(C.>+++++++<)
    8 -> return ''(C.>++++++++<)
    9 -> return ''(C.>+++++++++<)
    10 -> return ''(C.>++++++++++<)
    _ -> Nothing
bond n = fmap ConT $ case n of
    2 -> return ''(C.>|<)
    3 -> return ''(C.>||<)
    4 -> return ''(C.>|||<)
    5 -> return ''(C.>||||<)
    6 -> return ''(C.>|||||<)
    7 -> return ''(C.>||||||<)
    8 -> return ''(C.>|||||||<)
    9 -> return ''(C.>||||||||<)
    10 -> return ''(C.>|||||||||<)
    _ -> Nothing

-- | 'clussify' converts a type class into a cluss, roughly speaking.
-- For example, if the visible instances of 'Show'
-- were to be only @'Show' 'Int'@, @'Show' a => 'Show' [a]@, and @('Show' a, 'Show' b) => 'Show' (a, b)@,
-- the result of @$('clussify' \'\''Show')@ will be
--
-- >Show >|< In [Type Int, Unary [] Show, Binary (,) (Show >|< Show)]
--
-- (in fact, the result will be more verbose, using @'Show' \>|\< 'Pure' \>++\< 'Pure' \>|\< 'Show'@ instead of @'Show' \>|\< 'Show'@).
--
-- Due to the stage restriction of template haskell, 'clussify' can't catch the instances defined in the module where the 'classify' is written.
--
-- Note that 'clussify' neglects complicated instances that cannot be simply expressed with the combinators in the module <Type-Cluss.html Type.Cluss>.
--
-- You need some language extensions to use 'clussify'. Basically, this language pragma will do.
--
-- >{-# LANGUAGE TemplateHaskell, ConstraintKinds #-}
clussify :: Name -> Q Type
clussify nm = do
    info <- reify nm
    return (ConT ''(C.>+<) `appt` ConT nm `appt` convertInfo info)

convertInfo :: Info -> Type
convertInfo (ClassI _ idecs) = ConT ''C.In `appt` foldr
    (\typ1 typ2 -> PromotedConsT `appt` typ1 `appt` typ2) PromotedNilT (map fromJust . filter isJust . map convertIdec $ idecs)
convertInfo _ = error "Type.Cluss.TH.convertInfo: unsupported Info"

convertIdec :: InstanceDec -> Maybe Type
convertIdec (InstanceD prds (AppT (ConT _z) typ) _) = do
    typs <- foldr (\prd res -> do
        res' <- res
        ct <- convertPred tvs prd
        return $ ct : res') (return []) prds
    case n of
        0 -> return $ ConT ''C.Type `appt` typa
        _ -> do
            o <- ovrlp n
            p <- makeCnstrnt n n (ConT ''C.Pure)
            a <- nary n
            let typp = foldl (\typ1 typ2 -> o `appt` typ1 `appt` typ2) p typs
            return $ a `appt` typa `appt` typp
  where
    n = length tvs
    (typa, tvs) = convertType typ

convertIdec _ = error "Type.Cluss.TH.convertIdec: unsupported InstanceDec"

convertType :: Type -> (Type, [Name])
convertType (AppT typ (VarT tv)) = (typ', tvs ++ [tv])
  where
    (typ', tvs) = convertType typ
convertType (AppT typ (SigT (VarT tv) _)) = (typ', tv : tvs)
  where
    (typ', tvs) = convertType typ
convertType typ = (typ, [])

convertPred :: [Name] -> Pred -> Maybe Type
convertPred tvs (ClassP nm (typs@(_:_))) = do
    tv <- case last typs of
        VarT tv' -> return tv'
        _ -> Nothing
    let k = fromJust (elemIndex tv tvs)
    makeCnstrnt n k typ
  where
    n = length tvs
    typ = foldl (\typ' prm -> typ' `appt` prm) (ConT nm) (init typs)
convertPred tvs (EqualP typ (VarT tv)) = makeCnstrnt n k (ConT ''C.Is `appt` typ)
  where
    n = length tvs
    k = fromJust (elemIndex tv tvs)
convertPred _ _ = Nothing

makeCnstrnt :: Int -> Int -> Type -> Maybe Type
makeCnstrnt 1 0 t = return $ t
makeCnstrnt 1 _ _ = return $ ConT ''C.Pure
makeCnstrnt n k t = do
    b <- bond n
    m <- makeCnstrnt (n - 1) k t
    return $ b `appt` m `appt` c
  where
    c = if n -1 == k then t else ConT ''C.Pure
