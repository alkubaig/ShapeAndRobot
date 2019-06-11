{-# LANGUAGE
      DeriveFunctor,
      FlexibleContexts,
      MultiParamTypeClasses,
      TypeOperators
  #-}

module Rect where

import Prelude

import AlaCarte
import Prim
import Cond
import Shape
import ShapeArea

--
-- * Syntax
--

-- | Extend Shapes

data Rect t = Rec (Point t) t t
              deriving (Eq,Functor,Show)
              

rect  :: (Rect :<: t) => Point (Term t) -> Term t -> Term t  -> Term t
rect p h w = inject (Rec p h w)

--
-- * Extend Pretty printing
--

instance Pretty Rect where
  prettyAlg (Rec p h w) = concat ["rect: center ", prettyAlg p, " height: ", h, " width: ", w]

--
-- -- ** Extend Evaluation (of Shape)
--

evalRect ::   (ShapeDom :<: t,PVal :<: t) =>  Term t -> Term t -> Term t -> Term t -> Term t 
evalRect x y h w =  
  case (project x, project y,project h,project w ) of
          (Just (F x'), Just(F y'), Just(F h'), Just(F w') ) -> shapeDom ( P (float x')  (float y')) (float h') (float w')
          _ -> error "Type error: non-float values"

instance Eval Rect where
  evalAlg (Rec (P x y) h w)  =  evalRect x y h w 
 
 --
 -- ** Extend Area 
 -- 
 
instance Area Rect where
  areaAlg (Rec (P x y) h w) = 
    case (h, w, x,y) of
      (F h',F w', F _ , F _ ) -> F $ h' * w'
      _ -> error "Type error: non-float values"   



