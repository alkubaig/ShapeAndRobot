{-# LANGUAGE
      DeriveFunctor,
      FlexibleContexts,
      MultiParamTypeClasses,
      TypeOperators
  #-}

module Step where

import Prelude

import AlaCarte
import Prim
import Cond
import Shape
import ShapeArea
import Rect
import ShapeCircumference
import Move
import ShapeToMoves

--
-- * Syntax
--

-- | Extend Move


data Step t = Steps t
              deriving (Eq,Functor,Show)
              

steps  :: (Step :<: t) => Term t  -> Term t
steps = inject . Steps 

--
-- * Extend Pretty printing
--

instance Pretty Step where
  prettyAlg (Steps s) = concat ["Steps: ", s]

--
-- -- ** Extend Evaluation (of Move)
--

evalStep ::   (State :<: t,PVal :<: t) => (Term t) ->(Term t) -> Term t
evalStep s m = 
  case evalState m of
    (St d (P x y) ) ->  
       case (project (x ), project (y)) of 
         (Just (F x'), Just(F y')) -> 
           case project (s) of 
             (Just (F s')) ->  state d (updateLoc d (x',y') s') 
             _ -> error "Type error: non-float values"
         _ -> error "Type error: non-float values"

instance EvalM Step where
  evalAlgM (Steps s) m = evalStep (s m) m


instance ShapeToMoves Step where
  shapeToMovesAlg m = error "Type error: expecting a shape"  
  
--
-- * ShapeToMoves
--

-- hLineMoves' ::   (Move :<: t, PVal :<: t, Shape :<: t,Moves :<: t,Step :<: t) => Term t -> Term t -> Term t -> Term t
-- hLineMoves' y x1 x2    =
--       case (project y, project x1,project x2) of
--             (Just (F _), Just(F x1'), Just(F x2') ) ->
--                let leftB = jumpToCorner BottomLeft (hline y x1 x2) in
--                let dir = change RightD in
--                let d = dist x1' x2' in
--                let s = steps (float d)  in
--                movseq $ [leftB, dir, s ]
--             _ -> error "Type error: non-float values"
--
-- vLineMoves' ::   (Move :<: t, PVal :<: t, Shape :<: t,Moves :<: t,Step :<: t) => Term t -> Term t -> Term t -> Term t
-- vLineMoves' x y1 y2    =
--     case (project x, project y1,project y2 ) of
--           (Just (F x'), Just (F y1'), Just (F y2') ) ->
--                let leftB = jumpToCorner BottomLeft (vline x y1 y2) in
--                let dir = change Up in
--                let d = dist y1' y2' in
--                let s = steps (float d)  in
--                movseq $ [leftB, dir,s ]
--           _ -> error "Type error: non-float values"
--
-- squareMoves' ::   (Move :<: t, PVal :<: t, Shape :<: t,Moves :<: t,Step :<: t) => Term t -> Term t -> Term t -> Term t
-- squareMoves' x y l    =
--    case (project x, project y,project l) of
--            (Just (F x'), Just(F y'), Just(F l') ) ->
--              let leftB = jumpToCorner BottomLeft (square ( P x y) l) in
--              let s = steps (float l')  in
--              let dir1 = change RightD in
--              let dir2 = change Up in
--              let dir3 = change LeftD in
--              let dir4 = change Down in
--              movseq $ [leftB, dir1, s, dir2, s, dir3, s, dir4, s]
--            _ -> error "Type error: non-float values"
           

-- rectMoves' ::   (Move :<: t, PVal :<: t, Shape :<: t,Moves :<: t, Rect :<: t,Step :<: t) =>  Term t -> Term t -> Term t -> Term t -> Term t
-- rectMoves' x y h w =
--   case (project x, project y,project h,project w ) of
--           (Just (F x'), Just(F y'), Just(F h'), Just(F w') ) ->
--             let leftB = jumpToCorner BottomLeft (rect ( P x y) h w) in
--             let s1 = steps (float h')  in
--             let s2 = steps (float w')  in
--             let dir1 = change RightD in
--             let dir2 = change Up in
--             let dir3 = change LeftD in
--             let dir4 = change Down in
--             movseq $ [leftB, dir1, s2, dir2, s1, dir3, s2, dir4, s1]
--           _ -> error "Type error: non-float values"
--
--
-- instance ShapeToMoves Rect where
--   shapeToMovesAlg (Rec (P x y) h w) = rectMoves' x y h w
