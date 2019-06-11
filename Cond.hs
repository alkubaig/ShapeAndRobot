{-# LANGUAGE
      DeriveFunctor,
      FlexibleContexts,
      MultiParamTypeClasses,
      TypeOperators
  #-}

module Cond where

import AlaCarte
import Prim


--
-- * Syntax
--

-- | Extend Primitive Operations 
-- | Conditional

data Cond t = If t t t
  deriving (Eq,Functor,Show)

-- | Smart constructor.
cond :: (Cond :<: t) => Term t -> Term t -> Term t -> Term t
cond c t e = inject (If c t e)


--
-- * Extend Pretty printing
--

instance Pretty Cond where
  prettyAlg (If c t e) = unwords ["if", c, "then", t, "else", e, "end"]


--
-- * Extend Evaluation
--

evalCond ::  PVal t-> PVal t -> PVal t -> PVal t
evalCond c t e = 
  case c of
      B True  -> t
      B False -> e
      _ -> error "Type error: non-boolean condition"



instance PEval Cond where
  pevalAlg (If c t e) = evalCond c t e


