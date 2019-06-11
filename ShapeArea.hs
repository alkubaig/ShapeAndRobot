{-# LANGUAGE
      DeriveFunctor,
      FlexibleContexts,
      MultiParamTypeClasses,
      TypeOperators
  #-}

module ShapeArea where

import Prelude

import AlaCarte
import Prim
import Cond
import Shape

--
-- ** New Operation: Area
-- 

class Functor t => Area t  where
  areaAlg :: t (PVal ()) -> PVal ()

-- Boilerplate needed for each new interpretation.
instance (Area s1, Area s2) => Area (s1 :+: s2) where
  areaAlg (InL a) = areaAlg a
  areaAlg (InR b) = areaAlg b


area :: Area t => Term t -> PVal ()
area = foldTerm areaAlg

instance Area Prim where
  areaAlg (P1 o e)   = evalP1 o e
  areaAlg (P2 o l r) = evalP2 o l r
  
  
instance Area PVal where
  areaAlg (F f) = (F f)
  areaAlg (B b) = B b

instance Area Cond where
  areaAlg (If c t e) = evalCond c t e

instance Area Point where
  areaAlg (P x y) = F 1

instance Area Shape where
  areaAlg (Pt (P x y))    = areaAlg (P x y)
  areaAlg (Hline y x1 x2) = 
    case (y,x1, x2) of
      (F _, F x1', F x2') -> F $ abs (x2' - x1')
      _ -> error "Type error: non-float values"
    
  areaAlg (Vline x y1 y2) =
    case (x,y1, y2) of
      (F _, F y1', F y2') -> F $ abs (y2' - y1')
      _ -> error "Type error: non-float values"
  areaAlg (Square (P x y) l) = 
    case (l, x,y) of
      (F l', F _ , F _ ) -> F $ l' * l'
      _ -> error "Type error: non-float values"
      




