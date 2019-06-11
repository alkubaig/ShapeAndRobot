{-# LANGUAGE
      DeriveFunctor,
      FlexibleContexts,
      MultiParamTypeClasses,
      TypeOperators
  #-}

module Testing where

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
import Step

--
-- ** Examples
--

--
-- ** Prim
--

type PExpr = Term (Prim :+: PVal)
type PExpr' = Term (PVal :+: Prim)

ex1 :: (Prim :<: t, PVal :<: t) => Term t
ex1 = op2 Add (op2 Mul (float 2) (float 3)) (op1 Neg (float 4))

ex2 :: (Prim :<: t, PVal :<: t) => Term t
ex2 = op2 And (op2 LTE ex1 (float 3)) (bool True)


testEx1 = peval $ (ex1 :: PExpr)
testEx1' = pretty $ (ex1 :: PExpr)

testEx2 = peval $ (ex2 :: PExpr)
testEx2' = pretty $ (ex1 :: PExpr)


--
-- ** Cond
--

type CExpr = Term (Cond :+: Prim :+: PVal)

ex3 :: (Cond :<: t, Prim :<: t, PVal :<: t) => Term t
ex3 = cond (op2 LTE (float 3) (float 4)) ex1 ex2

testEx3 = peval $ (ex3 :: CExpr)
testEx3' = pretty $ (ex3 :: CExpr)


-- 
-- ** Shape
--

type SExpr = Term (Cond :+: Prim :+: PVal :+: Shape :+: Point )

ex4 :: (Cond :<: t, Prim :<: t, PVal :<: t , Shape :<: t, Point :<: t) => Term t
ex4 = (hline (float 3) (float 5) ex1)

ex5 :: (Cond :<: t, Prim :<: t, PVal :<: t , Shape :<: t, Point :<: t) => Term t
ex5 = (square (point (float 3) (float 4)) ex1)

ex6 :: (Cond :<: t, Prim :<: t, PVal :<: t , Shape :<: t, Point :<: t) => Term t
ex6 = (square (point (float 3) (float 4)) ex2)

ex7 :: (Cond :<: t, Prim :<: t, PVal :<: t , Shape :<: t, Point :<: t) => Term t
ex7 = (vline (float 3) (float 7) ex1)


testEx4 = pretty $ eval (ex4 :: SExpr)
testEx4' = pretty (ex4 :: SExpr)

testEx5 = pretty $ eval (ex5 :: SExpr)
testEx5' = pretty (ex5 :: SExpr)

testEx6 = pretty $ eval (ex6 :: SExpr)
testEx6' = pretty (ex6 :: SExpr)

testEx7 = pretty $ eval (ex7 :: SExpr)
testEx7' = pretty (ex7 :: SExpr)

-- 
-- ** ShapeArea
--

testEx4'' = area (ex4 :: SExpr)
testEx5'' = area (ex5 :: SExpr)
testEx6'' = area (ex6 :: SExpr)
testEx7'' = area (ex7 :: SExpr)


-- 
-- ** Rect
--

type RExpr = Term (Cond :+: Prim :+: PVal :+: Shape :+: Point :+: Rect )

ex8 :: (Cond :<: t, Prim :<: t, PVal :<: t , Shape :<: t, Point :<: t , Rect :<: t) => Term t
ex8 = (rect (point (float 3) (float 4)) (float 7) ex1)

ex9 :: (Cond :<: t, Prim :<: t, PVal :<: t , Shape :<: t, Point :<: t, Rect :<: t) => Term t
ex9 = (rect (point (float 3) (float 4)) ex1 ex2)

testEx8 = pretty $ eval (ex8 :: RExpr)
testEx8' = pretty (ex8 :: RExpr)
testEx8'' = area (ex8 :: RExpr)

testEx9 = pretty $ eval (ex9 :: RExpr)
testEx9' = pretty (ex9 :: RExpr)
testEx9'' = area (ex9 :: RExpr)


-- 
-- ** ShapeCircumference
--

testEx4_'' = circumference (ex4 :: SExpr)
testEx5_'' = circumference (ex5 :: SExpr)
testEx6_'' = circumference (ex6 :: SExpr)
testEx7_'' = circumference (ex7 :: SExpr)
testEx8_'' = circumference (ex8 :: RExpr)
testEx9_'' = circumference (ex9 :: RExpr)

-- 
-- ** Move
--

type MExpr = Term (Cond :+: Prim :+: PVal :+: Shape :+: Point :+: Rect  :+: Move :+: Moves)

ex10 :: (Cond :<: t, Prim :<: t, PVal :<: t , Shape :<: t, Point :<: t , Rect :<: t, Move :<: t) => Term t
ex10 = (jumpTo (point (float 3) (float 5) ) )

ex11 :: (Cond :<: t, Prim :<: t, PVal :<: t , Shape :<: t, Point :<: t , Rect :<: t, Move :<: t) => Term t
ex11 = (jumpToCorner BottomLeft ex8 )

ex12 :: (Cond :<: t, Prim :<: t, PVal :<: t , Shape :<: t, Point :<: t , Rect :<: t, Move :<: t) => Term t
ex12 = go

ex13 :: (Cond :<: t, Prim :<: t, PVal :<: t , Shape :<: t, Point :<: t , Rect :<: t, Move :<: t, Moves :<: t) => Term t
ex13 = movseq [ ex10, ex11, ex12]

testEx10 = pretty $ evalM (ex10 :: MExpr)
testEx10' = pretty (ex10 :: MExpr)

testEx11 = pretty $ evalM (ex11 :: MExpr)
testEx11' = pretty (ex11 :: MExpr)

testEx12 = pretty $ evalM (ex12 :: MExpr)
testEx12' = pretty (ex12 :: MExpr)

testEx13 = pretty $ evalM (ex13 :: MExpr)
testEx13' = pretty (ex13 :: MExpr)


--
-- ** ShapeToMoves
--

testEx4M = pretty $ shapeToMoves (ex4 :: MExpr)
testEx4ME = pretty $ evalM $ shapeToMoves (ex4 :: MExpr)

testEx5M = pretty $ shapeToMoves (ex5 :: MExpr)
testEx5ME = pretty $ evalM $ shapeToMoves (ex5 :: MExpr)

testEx6M = pretty $ shapeToMoves (ex6 :: MExpr)
testEx6ME = pretty $ evalM $ shapeToMoves (ex6 :: MExpr)

testEx7M = pretty $ shapeToMoves (ex7 :: MExpr)
testEx7ME = pretty $ evalM $ shapeToMoves (ex7 :: MExpr)

testEx8M = pretty $ shapeToMoves (ex8 :: MExpr)
testEx8ME = pretty $ evalM $ shapeToMoves (ex8 :: MExpr)

testEx9M = pretty $ shapeToMoves (ex9 :: MExpr)
testEx9ME = pretty $ evalM $ shapeToMoves (ex9 :: MExpr)


--
-- ** Steps
--

type SMExpr = Term (Cond :+: Prim :+: PVal :+: Shape :+: Point :+: Rect  :+: Move  :+: Moves  :+: Step)

ex14 :: (Cond :<: t, Prim :<: t, PVal :<: t , Shape :<: t, Point :<: t , Rect :<: t, Move :<: t , Moves :<: t , Step :<: t) => Term t
ex14 = (steps (float 5) )

ex15 :: (Cond :<: t, Prim :<: t, PVal :<: t , Shape :<: t, Point :<: t , Rect :<: t, Move :<: t , Moves :<: t , Step :<: t) => Term t
ex15 = movseq [ ex10, ex11, ex12,ex14]

testEx14 = pretty $ evalM (ex14 :: SMExpr)
testEx14' = pretty (ex14 :: SMExpr)

testEx15 = pretty $ evalM (ex15 :: SMExpr)
testEx15' = pretty (ex15 :: SMExpr)
