{-# OPTIONS_GHC -Wall -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Dvda.MSTHExample where

import Dvda.MSTH
import Dvda.Expr (sym, Expr(..))

import Data.Array.Repa (DIM0, Z)

xs = [sym "22", sym "33"]
us = [sym "aaa"]

data ODEProblem a = ODEProblem { odepNstate :: Int
                               , odepNaction :: Int
                               , odepNparams :: Int
                               , odepParams :: [Expr Z a]
                               , odepOdefun :: Expr Z a -> Expr Z a -> Expr Z a }
-- | This _should_ work, but there's some parse error that isn't
-- explained.
myODE xs us = [ode|eqn|]
    where eqn = [v, -k*x - b*v - u]
  [action| us : u|]
   [state| xs : x v|]
          -- [param| k b |]

-- [param| k b |]
-- [action| us : u|]

