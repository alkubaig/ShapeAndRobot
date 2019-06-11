{-# LANGUAGE
      DeriveFunctor,
      FlexibleContexts,
      MultiParamTypeClasses,
      TypeOperators
  #-}

module ShapeToMoves where

import Prelude

import AlaCarte
import Prim
import Cond
import Shape
import ShapeArea
import Rect
import ShapeCircumference
import Move

  
--
-- ** New Operation: ShapeToMoves
-- 
 
type MovVal = Term (PVal :+: Shape :+: Move :+: Moves :+: Rect )
 

class Functor t => ShapeToMoves t  where
  shapeToMovesAlg :: t (MovVal) -> MovVal

-- Boilerplate needed for each new interpretation.
instance (ShapeToMoves s1, ShapeToMoves s2) => ShapeToMoves (s1 :+: s2) where
  shapeToMovesAlg (InL a) = shapeToMovesAlg a
  shapeToMovesAlg (InR b) = shapeToMovesAlg b
  

shapeToMoves :: ShapeToMoves t => Term t -> MovVal
shapeToMoves t = foldTerm shapeToMovesAlg t


instance ShapeToMoves Prim where
  shapeToMovesAlg (P1 o e) = inject $ evalPrimP1 o e
  shapeToMovesAlg (P2 o l r) = inject $ evalPrimP2 o l r

instance ShapeToMoves Cond where
  shapeToMovesAlg (If c t e) = evalCon c t e

instance ShapeToMoves PVal where
  shapeToMovesAlg (B b) = bool b
  shapeToMovesAlg (F f) = float f

instance ShapeToMoves Point where
  shapeToMovesAlg (P x y) = movseq [jumpTo (P x y)]


goes ::  (Move :<: t)  => Float -> Moves (Term t)
goes 0 = L []
goes n = let ( L l') = goes (n-1) in L (go : l') 


hLineMoves ::   (Move :<: t, PVal :<: t, Shape :<: t,Moves :<: t) => Term t -> Term t -> Term t -> Term t
hLineMoves y x1 x2    = 
      case (project y, project x1,project x2) of
            (Just (F _), Just(F x1'), Just(F x2') ) ->
               let leftB = jumpToCorner BottomLeft (hline y x1 x2) in
               let dir = change RightD in
               let d = dist x1' x2' in
               let (L steps) = goes d in
               movseq $ [leftB, dir ] ++ steps
            _ -> error "Type error: non-float values"
            
vLineMoves ::   (Move :<: t, PVal :<: t, Shape :<: t,Moves :<: t) => Term t -> Term t -> Term t -> Term t
vLineMoves x y1 y2    = 
    case (project x, project y1,project y2 ) of
          (Just (F x'), Just (F y1'), Just (F y2') ) ->
               let leftB = jumpToCorner BottomLeft (vline x y1 y2) in
               let dir = change Up in
               let d = dist y1' y2' in 
               let (L steps) = goes d in
               movseq $ [leftB, dir ] ++ steps
          _ -> error "Type error: non-float values"
          
squareMoves ::   (Move :<: t, PVal :<: t, Shape :<: t,Moves :<: t) => Term t -> Term t -> Term t -> Term t
squareMoves x y l    = 
   case (project x, project y,project l) of
           (Just (F x'), Just(F y'), Just(F l') ) -> 
             let leftB = jumpToCorner BottomLeft (square ( P x y) l) in
             let (L steps) = goes l' in
             let dir1 = change RightD in
             let dir2 = change Up in
             let dir3 = change LeftD in
             let dir4 = change Down in
             movseq $ [leftB, dir1 ] ++ steps ++ [dir2] ++ steps ++ [dir3] ++ steps ++ [dir4] ++ steps
           _ -> error "Type error: non-float values"
           

rectMoves ::   (Move :<: t, PVal :<: t, Shape :<: t,Moves :<: t, Rect :<: t) =>  Term t -> Term t -> Term t -> Term t -> Term t 
rectMoves x y h w =  
  case (project x, project y,project h,project w ) of
          (Just (F x'), Just(F y'), Just(F h'), Just(F w') ) -> 
            let leftB = jumpToCorner BottomLeft (rect ( P x y) h w) in
            let (L steps1) = goes h' in
            let (L steps2) = goes w' in
            let dir1 = change RightD in
            let dir2 = change Up in
            let dir3 = change LeftD in
            let dir4 = change Down in
            movseq $ [leftB, dir1 ] ++ steps2 ++ [dir2] ++ steps1 ++ [dir3] ++ steps2 ++ [dir4] ++ steps1
          _ -> error "Type error: non-float values"

instance ShapeToMoves Shape where
  shapeToMovesAlg (Pt (P x y))    = shapeToMovesAlg (P x y)
  shapeToMovesAlg (Hline y x1 x2) = hLineMoves y x1 x2
  shapeToMovesAlg (Vline x y1 y2) = vLineMoves x y1 y2
  shapeToMovesAlg (Square (P x y) l) = squareMoves x y l

instance ShapeToMoves Rect where
  shapeToMovesAlg (Rec (P x y) h w) = rectMoves x y h w

instance ShapeToMoves Move where
  shapeToMovesAlg m = error "Type error: expecting a shape"
  
instance ShapeToMoves Moves where
  shapeToMovesAlg m = error "Type error: expecting a shape"
            
