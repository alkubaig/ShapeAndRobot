{-# LANGUAGE
      DeriveFunctor,
      FlexibleContexts,
      MultiParamTypeClasses,
      TypeOperators
  #-}

module Prim where

import AlaCarte


--
-- * Syntax
--

-- | Primitive values.
data PVal t
   = B Bool
   | F Float
  deriving (Eq,Functor,Show)

-- | Unary operations.
data Op1 = Not | Neg
  deriving (Eq,Show)

-- | Binary operations.
data Op2 = And | Or | Add | Mul | LTE
  deriving (Eq,Show)

-- | Primitive operations.
data Prim t
   = P1 Op1 t
   | P2 Op2 t t
  deriving (Eq,Functor,Show)

bool :: (PVal :<: t) => Bool -> Term t
bool = inject . B

float :: (PVal :<: t) => Float -> Term t
float = inject . F

op1 :: (Prim :<: t) => Op1 -> Term t -> Term t
op1 o t = inject (P1 o t)

op2 :: (Prim :<: t) => Op2 -> Term t -> Term t -> Term t
op2 o l r = inject (P2 o l r)


--
-- * Pretty printing
--

-- | Signature for pretty printing.
class Functor t => Pretty t where
  prettyAlg :: t String -> String

-- Boilerplate needed for each new interpretation.
instance (Pretty s1, Pretty s2) => Pretty (s1 :+: s2) where
  prettyAlg (InL a) = prettyAlg a
  prettyAlg (InR b) = prettyAlg b

-- | Pretty printing.
pretty :: Pretty t => Term t -> IO ()
pretty = putStrLn . foldTerm prettyAlg

prettyOp1 :: Op1 -> String
prettyOp1 Not = "!"
prettyOp1 Neg = "-"

prettyOp2 :: Op2 -> String
prettyOp2 And = " & "
prettyOp2 Or  = " | "
prettyOp2 Add = " + "
prettyOp2 Mul = " * "
prettyOp2 LTE = " ≤ "

instance Pretty Prim where
  prettyAlg (P1 o e)   = prettyOp1 o ++ e
  prettyAlg (P2 o l r) = concat ["(", l, prettyOp2 o, r, ")"]

instance Pretty PVal where
  prettyAlg (B b) = show b
  prettyAlg (F f) = show f


--
-- * Evaluation
--

-- | Signature for primitive evaluation semantics.
class Functor t => PEval t where
  pevalAlg :: t (PVal ()) -> PVal ()

-- Boilerplate needed for each new interpretation.
instance (PEval s1, PEval s2) => PEval (s1 :+: s2) where
  pevalAlg (InL a) = pevalAlg a
  pevalAlg (InR b) = pevalAlg b

-- | Primitive evaluation.
peval :: PEval t => Term t -> PVal ()
peval = foldTerm pevalAlg

-- | Evaluate a primitive unary operation.
evalP1 :: Op1 -> PVal t -> PVal t
evalP1 Not (B b) = B (not b)
evalP1 Neg (F f) = F (negate f)
evalP1 o   v     = error (unwords ["Type error:", show o, show v])

-- | Evaluate a primitive binary operation.
evalP2 :: Op2 -> PVal t -> PVal t -> PVal t
evalP2 And (B l) (B r) = B (l && r)
evalP2 Or  (B l) (B r) = B (l || r)
evalP2 Add (F l) (F r) = F (l + r)
evalP2 Mul (F l) (F r) = F (l * r)
evalP2 LTE (F l) (F r) = B (l <= r)
evalP2 o   l     r     = error (unwords ["Type error:", show o, show l, show r])

instance PEval Prim where
  pevalAlg (P1 o e)   = evalP1 o e
  pevalAlg (P2 o l r) = evalP2 o l r

instance PEval PVal where
  pevalAlg (B b) = B b
  pevalAlg (F f) = F f


