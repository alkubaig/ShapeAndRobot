{-# LANGUAGE
      DeriveFunctor,
      FlexibleContexts,
      MultiParamTypeClasses,
      TypeOperators
  #-}

module ShapeCircumference where

import Prelude

import AlaCarte
import Prim
import Cond
import Shape
import ShapeArea
import Rect

 --
 -- ** New Operation: Circumference 
 -- 
 
class Functor t => Circumference t  where
  circumferenceAlg :: t (PVal ()) -> PVal ()

-- Boilerplate needed for each new interpretation.
instance (Circumference s1, Circumference s2) => Circumference (s1 :+: s2) where
  circumferenceAlg (InL a) = circumferenceAlg a
  circumferenceAlg (InR b) = circumferenceAlg b


circumference :: Circumference t => Term t -> PVal ()
circumference = foldTerm circumferenceAlg

instance Circumference Prim where
  circumferenceAlg (P1 o e)   = evalP1 o e
  circumferenceAlg (P2 o l r) = evalP2 o l r

instance Circumference PVal where
  circumferenceAlg (F f) = (F f)
  circumferenceAlg (B b) = B b

instance Circumference Cond where
  circumferenceAlg (If c t e) = evalCond c t e

instance Circumference Point where
  circumferenceAlg (P x y) = areaAlg (P x y)

instance Circumference Shape where
  circumferenceAlg (Pt (P x y))    = areaAlg (P x y)
  circumferenceAlg (Hline y x1 x2) = areaAlg (Hline y x1 x2)
  circumferenceAlg (Vline x y1 y2) = areaAlg (Vline x y1 y2)
  circumferenceAlg (Square (P x y) l) =
    case (l, x,y) of
      (F l', F _ , F _ ) -> F $ 4 * l'
      _ -> error "Type error: non-float values"
 
instance Circumference Rect where
  circumferenceAlg (Rec (P x y) h w) = 
    case (h, w, x,y) of
      (F h',F w', F _ , F _ ) -> F $ (2 * h') + (2 * w')
      _ -> error "Type error: non-float values"   






