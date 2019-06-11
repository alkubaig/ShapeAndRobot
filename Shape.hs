{-# LANGUAGE
      DeriveFunctor,
      FlexibleContexts,
      MultiParamTypeClasses,
      TypeOperators
  #-}

module Shape where

import Prelude

import AlaCarte
import Prim
import Cond


--
-- * Syntax
--

data Point t = P t t 
              deriving (Eq,Functor,Show)


data Shape t = Pt (Point t )
            | Hline t t t 
            | Vline t t t 
            | Square (Point t) t
              deriving (Eq,Functor,Show)
              
             
point  :: (Point :<: t) =>  Term t -> Term t -> Point (Term t)
point  t1 t2 = P t1 t2

pt  :: (Shape :<: t) => Point (Term t) -> Term t
pt = inject . Pt

hline  :: (Shape :<: t) => Term t -> Term t -> Term t-> Term t
hline y x1 x2 = inject (Hline y x1 x2)

vline  :: (Shape :<: t) => Term t -> Term t -> Term t-> Term t
vline x y1 y2 = inject (Vline x y1 y2)

square  :: (Shape :<: t) => Point (Term t) -> Term t -> Term t
square p l = inject (Square p l)


--
-- * Pretty printing
--

instance Pretty Point where
  prettyAlg (P x y)   = concat ["(", x, ", ", y, ")"]

instance Pretty Shape where
  prettyAlg (Pt p)   = concat ["pt: ", prettyAlg p]
  prettyAlg (Hline y x1 x2) = concat ["line: from ", prettyAlg (P (min x1 x2) y) , " to ", prettyAlg (P (max x1 x2) y)]
  prettyAlg (Vline x y1 y2) = concat ["line: from ", prettyAlg (P x (min y1 y2)) , " to ", prettyAlg (P x (max y1 y2))]
  prettyAlg (Square p l) = concat ["sqr: center ", prettyAlg p, " side len: ", l]

--
-- -- ** New Evaluation semantics
--
-- | A value is a primitive value or (center, height, width)
type Value = Term (PVal :+: ShapeDom)

-- | Signature for evaluation semantics.
class Functor t => Eval t where
  evalAlg :: t Value -> Value

data ShapeDom t = S (Point t) t t
  deriving Functor

shapeDom  :: (ShapeDom :<: t) => Point (Term t) -> Term t -> Term t -> Term t
shapeDom c h w = inject (S c h w)

instance Pretty ShapeDom where
  prettyAlg (S c h w ) = concat ["center: ", prettyAlg c , " height: ", h, " width: ", w ]


-- Boilerplate needed for each new interpretation.
instance (Eval s1, Eval s2) => Eval (s1 :+: s2) where
  evalAlg (InL a) = evalAlg a
  evalAlg (InR b) = evalAlg b

-- | Evaluate to a value.
eval :: Eval t => Term t -> Value
eval t = foldTerm evalAlg t

  
--
-- -- ** Evaluation operation
--    

evalPrimP1 ::  (PVal :<: t) => Op1 -> Term t-> PVal ( Term t)
evalPrimP1 o e =  
  case project e of
      Just pv -> evalP1 o pv
      _ -> error "Type error: unary operator applied to non-primitive value"

evalPrimP2 ::  (PVal :<: t) =>  Op2 -> Term t -> Term t -> PVal ( Term t)
evalPrimP2 o l r  =  
  case (project l, project r) of
      (Just pl, Just pr) -> evalP2 o pl pr
      _ -> error "Type error: binary operator applied to non-primitive value"
      

evalCon ::  (PVal :<: t) => Term t -> Term t -> Term t -> Term t
evalCon c t e =  
    case (project c) of
      Just (B b) -> if b then t else e
      _ -> error "Type error: non-boolean condition"


evalPoint ::   (ShapeDom :<: t,PVal :<: t) => Term t ->  Term t -> Term t
evalPoint x y =
  case (project x, project y) of
    (Just (F x'), Just (F y')) -> shapeDom ( P (float x') (float y')) (float 0) (float 0)
    _ -> error "Type error: non-float values"
    
    
dist:: Float -> Float -> Float
dist p1 p2 = abs (p2 - p1)

center :: Float -> Float -> Float
center p1 p2 = ((p1+ p2) / 2.0)


evalShapeHline ::   (ShapeDom :<: t,PVal :<: t) => Term t -> Term t -> Term t -> Term t
evalShapeHline y x1 x2    = 
      case (project y, project x1,project x2) of
            (Just (F y'), Just(F x1'), Just(F x2') ) -> shapeDom ( P (float $ center x1' x2') (float y')) (float 0) (float $ dist x2' x1')
            _ -> error "Type error: non-float values"

evalShapeVline ::   (ShapeDom :<: t,PVal :<: t) => Term t -> Term t -> Term t -> Term t
evalShapeVline x y1 y2    = 
    case (project x, project y1,project y2 ) of
          (Just (F x'), Just (F y1'), Just (F y2') ) -> shapeDom ( P (float x') (float $ center y1' y2')) (float $ dist y2' y1') (float 0)
          _ -> error "Type error: non-float values"

evalShapeSquare ::   (ShapeDom :<: t,PVal :<: t) => Term t -> Term t -> Term t -> Term t
evalShapeSquare x y l    = 
   case (project x, project y,project l) of
           (Just (F x'), Just(F y'), Just(F l') ) -> shapeDom ( P (float x')  (float y')) (float l') (float l')
           _ -> error "Type error: non-float values"
 

instance Eval Prim where
  evalAlg (P1 o e) = inject $ evalPrimP1 o e
  evalAlg (P2 o l r) = inject $ evalPrimP2 o l r

instance Eval Cond where
  evalAlg (If c t e) = evalCon c t e

instance Eval PVal where
  evalAlg (B b) = bool b
  evalAlg (F f) = float f

instance Eval Point where
  evalAlg (P x y) = evalPoint x y

instance Eval Shape where
  evalAlg (Pt (P x y))    = evalPoint x y 
  evalAlg (Hline y x1 x2) = evalShapeHline y x1 x2
  evalAlg (Vline x y1 y2) = evalShapeVline x y1 y2 
  evalAlg (Square (P x y) l) = evalShapeSquare x y l 
  
