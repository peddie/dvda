{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

-----------------------------------------------------------------------------
-- |
-- Module      :  Dvda.MSTH
-- Copyright   :  (c) Matthew Peddie 2012
-- License     :  GPLv3 (see the file dvda/LICENSE)
-- 
-- Maintainer  :  gregmainland@gmail.com
-- Stability   :  experimental
-- Portability :  GHC
--
-- Boilerplate reduction for multiple-shooting problem setup.
-----------------------------------------------------------------------------

module Dvda.MSTH where
                  
import qualified Data.IntMap as IM
import Data.Array.Repa ()
import Data.List (groupBy)

import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote

import Dvda
import Dvda.Expr (sym, Expr(..))
import Dvda.SparseLA ()
import Dvda.MS (dynamicsErrorsEuler)
-- import Dvda.SymMonad ( KeyT )
-- import Dvda.MSDSL

-- | Generic TH error generator.
thErr :: String -> Q a
thErr errstring = do loc <- TH.location
                     let fn = TH.loc_filename loc
                         lineno = fst $ TH.loc_start loc
                         colno = snd $ TH.loc_start loc
                     error $ errstring ++ " at " ++ show fn ++ ":" ++ show lineno ++ ":" ++ show colno

-- | We use nsErr to generate errors if you ask one of these
-- mini-parsers to run in the wrong context.
nsErr :: String -> String -> Q a
nsErr es _ = thErr (es ++ " not supported")

experr :: String -> Q Exp
experr = nsErr "Expressions"

paterr :: String -> Q Pat
paterr = nsErr "Patterns"

typeerr :: String -> Q Type
typeerr = nsErr "Types"

decerr :: String -> Q [Dec]
decerr = nsErr "Declarations"

-- | Generate a type signature for a symbolic expr declaration
typeSigParam :: Name -> Q Dec
typeSigParam x = do a <- newName "a"
                    return $ SigD x (ForallT [PlainTV a] [] (AppT (AppT (ConT $ mkName "Expr") (ConT $ mkName "DIM0")) (VarT a)))

-- | Generate a symbolic expr declaration
assignSymParam :: Name -> Dec
assignSymParam x = ValD (VarP x) (NormalB (AppE (VarE (mkName "Dvda.Expr.sym")) (LitE (StringL $ nameBase x)))) []

-- | Parameter declaration parser and code generator.
paramdec :: String -> Q [Dec]
paramdec s = do let names = map mkName $ words s
                sigs <- mapM typeSigParam names
                let defs = map assignSymParam names
                    nparam = assignSize (mkName "__nparams") (length defs)
                    params = ValD (VarP (mkName "__params")) (NormalB (ListE $ map VarE names)) []
                return $ nparam : params : sigs ++ defs

param :: QuasiQuoter
param = QuasiQuoter experr paterr typeerr paramdec

assignState :: Name -> [Name] -> Dec
assignState sv sn = ValD (ListP $ map VarP sn) (NormalB (VarE sv)) []

assignSize :: Name -> Int -> Dec
assignSize nsv nl = ValD (VarP nsv) (NormalB (LitE (IntegerL $ fromIntegral nl))) []

splitdec :: String -> String -> Q [Dec]
splitdec name s = case span (/= ':') s of
                    (sv, ':':sn) -> return [assignState sv' sn', assignSize (mkName $ "__n" ++ name) (length sn')]
                        where
                          sv' = head $ map mkName $ words sv
                          sn' = map mkName $ words sn
                    _ -> thErr $ "Malformed " ++ name ++ " definition '" ++ s ++ "',"

splitOn :: Char -> String -> [String]
splitOn c s = case span (/= c) s of
                (hs, []) -> [hs]
                (hs, c:ts) -> hs : splitOn c ts

-- | A mini code generator for breaking down a state vector into
-- individually labeled states.  Also generates @__nstate@ for
-- counting up the states.
state :: QuasiQuoter
state = QuasiQuoter experr paterr typeerr (splitdec "state")

-- | A mini code generator for breaking down an action vector into
-- individually labeled actions.  Also generates @__naction@ for
-- counting up the actions.
action :: QuasiQuoter
action = QuasiQuoter experr paterr typeerr (splitdec "action")

odeexp :: String -> Q Exp
odeexp s = return $ AppE (AppE (AppE (AppE (AppE (ConE (mkName "ODEProblem")) (VarE (mkName "__nstate"))) (VarE (mkName "__naction"))) (VarE (mkName "__nparams"))) (VarE (mkName "__params"))) (VarE (mkName s))

-- | A mini code generator for exporting an @ODEProblem@.  Relies on
-- scope capture for @__nstate@, @__naction@, @__params@ and
-- @__nparams@.
ode :: QuasiQuoter
ode = QuasiQuoter odeexp paterr typeerr decerr
