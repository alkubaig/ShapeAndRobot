{-# LANGUAGE
      DeriveFunctor,
      FlexibleContexts,
      MultiParamTypeClasses,
      TypeOperators
  #-}

module Move where

import Prelude

import AlaCarte
import Prim
import Cond
import Shape
import ShapeArea
import Rect
import ShapeCircumference

--
-- * Syntax
--


--
-- * Robotic Moves 
--

data Dir = Up | Down | LeftD | RightD 
              deriving (Eq,Show)

data ShapeCorner = TopLeft | TopRight |  BottomRight | BottomLeft
              deriving (Eq,Show)
      
data Move t = Go 
            | JumpTo (Point t) 
            | JumpToCorner ShapeCorner t 
            | Change Dir 
              deriving (Eq,Functor,Show)
              
data Moves t = L [t]
              deriving (Eq,Functor,Show)

movseq ::(Moves :<: t) => [(Term t)] -> Term t
movseq =  inject . L


go  :: (Move :<: t) => Term t
go = inject Go

jumpTo  :: (Move :<: t) => Point (Term t) -> Term t
jumpTo = inject . JumpTo 

jumpToCorner :: (Move :<: t) => ShapeCorner -> Term t -> Term t
jumpToCorner c s = inject (JumpToCorner c s)

change  :: (Move :<: t) => Dir -> Term t
change = inject . Change

--
-- * Extend Pretty printing
--

printDir :: Dir -> String
printDir d = show d  

printCorner :: ShapeCorner -> String
printCorner d = show d  

instance Pretty Move where
  prettyAlg m@Go   = show m
  prettyAlg (JumpTo p) = concat ["jump to ", prettyAlg p]
  prettyAlg (JumpToCorner c s) = concat ["jump to ", printCorner c , " corner of ", s]  
  prettyAlg (Change d) = concat ["change dir to ", show d]
  
instance Pretty Moves where
  prettyAlg (L [])   = "\n"
  prettyAlg (L (x:[]))   = x
  prettyAlg (L (x:xs)) = concat [x, " ; ", prettyAlg (L xs) ]  
  
--
-- -- ** New Evaluation semantics
--

-- | A value is a primitive value or (center, height, width) or state
type ValueM = Term (PVal :+: ShapeDom  :+: State)


-- | Semantic domain for evaluation.
type EvalSem = ValueM -> ValueM


-- | Signature for evaluation semantics.
class Functor t => EvalM t where
  evalAlgM :: t EvalSem -> EvalSem


data State t = St Dir (Point t)
              deriving (Eq,Functor,Show)

state  :: (State :<: t) => Dir -> Point (Term t) -> Term t
state d p = inject (St d p )


instance Pretty State where
  prettyAlg (St d p ) = concat ["(Dir: ", printDir d, " , Loc: " , prettyAlg p , ")" ]


-- Boilerplate needed for each new interpretation.
instance (EvalM s1, EvalM s2) => EvalM (s1 :+: s2) where
  evalAlgM (InL a) = evalAlgM a
  evalAlgM (InR b) = evalAlgM b


start = (P (float 0) (float 0))
intialState = (state RightD start)


-- | Evaluate to a value.
evalM :: EvalM t => Term t -> ValueM
evalM t = foldTerm evalAlgM t intialState


--
-- -- ** New Evaluation operation
--  

instance EvalM Prim where
  evalAlgM (P1 o e) m = inject $ evalPrimP1 o (e m)
  evalAlgM (P2 o l r) m  = inject $ evalPrimP2 o (l m) (r m)

instance EvalM Cond where
  evalAlgM (If c t e) m = evalCon (c m) (t m) (e m)

instance EvalM PVal where
  evalAlgM (B b) m = bool b
  evalAlgM (F f) m = float f

instance EvalM Point where
  evalAlgM (P x y) m = evalPoint (x m) (y m)

instance EvalM Shape where
  evalAlgM (Pt (P x y))       m = evalPoint (x m) (y m)
  evalAlgM (Hline y x1 x2)   m = evalShapeHline (y m) (x1 m) (x2 m)
  evalAlgM (Vline x y1 y2)   m = evalShapeVline (x m) (y1 m) (y2 m)
  evalAlgM (Square (P x y) l) m = evalShapeSquare (x m) (y m) (l m)

instance EvalM Rect where
  evalAlgM (Rec (P x y) h w)  m =  evalRect (x m) (y m) (h m) (w m)

updateLoc :: (PVal :<: t) => Dir -> (Float, Float) -> Float -> Point (Term t)
updateLoc Up (x,y) s = ( P (float x)  (float $ y+ s))
updateLoc Down (x,y) s = ( P (float x)  (float $ y- s))
updateLoc RightD (x,y) s = ( P (float $ x + s)  (float y))
updateLoc LeftD (x,y) s = ( P (float $ x - s)  (float y))


evalState :: (State :<: t,PVal :<: t) =>  Term t -> State(Term t)
evalState m =
    case (project m) of
       Just (St d (P x y)) -> St  d (P x y) 
       _ -> error "Type error: non valid state"

evalGo ::  (State :<: t,PVal :<: t) =>  Term t -> Term t
evalGo m =
  case evalState m of
      (St d (P x y) ) ->
         case (project (x ), project (y)) of
           (Just (F x'), Just(F y')) -> state d (updateLoc d (x',y') 1) 
           _ -> error "Type error: non-float values"

evalChange ::  (State :<: t,PVal :<: t) => Dir ->  Term t -> Term t
evalChange d' m =
     case evalState m of
        (St d p ) -> state d' p


evalJumpTo ::  (State :<: t,PVal :<: t) => Point (Term t) ->  Term t -> Term t
evalJumpTo (P x y ) m =
     case evalState m of
        (St d p ) ->
         case (project (x ), project (y )) of
           (Just (F x'), Just(F y')) -> state d ( P (float x')  (float y')) 
           _ -> error "Type error: non-float values"


findCorner :: (State :<: t, PVal :<: t) => ShapeCorner ->  Point (Term t) -> Term t ->  Term t -> Point (Term t)
findCorner c (P x y) h w =
   case (project x , project y, project h, project w) of
     (Just (F x'), Just(F y'),Just (F h'),Just (F w')) -> cornerLocation c x' y' h' w'
     _ ->  error "Type error: non-float values"

cornerLocation :: (State :<: t, PVal :<: t) =>  ShapeCorner -> Float ->Float -> Float ->Float -> Point (Term t)
cornerLocation (TopLeft ) = topLeft
cornerLocation (TopRight) = topRight
cornerLocation (BottomRight) = bottomRight
cornerLocation (BottomLeft) = bottomLeft


top ::  Float ->Float -> Float
top y h = y+(h/2)

bottom ::  Float ->Float -> Float
bottom y h = y-(h/2) 

left ::  Float ->Float -> Float
left x w = x-(w/2)

right ::  Float ->Float -> Float
right x w  = x+(w/2)

bottomLeft :: (State :<: t, PVal :<: t) => Float ->Float -> Float ->Float -> Point (Term t)
bottomLeft x y h w = floatP (left x w) (bottom y h)
  
bottomRight :: (State :<: t, PVal :<: t) => Float ->Float -> Float ->Float -> Point (Term t)
bottomRight x y h w = floatP (right x w) (bottom y h)
  
topRight :: (State :<: t, PVal :<: t) => Float ->Float -> Float ->Float -> Point (Term t)
topRight x y h w =  floatP (right x w) (top y h)
  
topLeft :: (State :<: t, PVal :<: t) => Float ->Float -> Float ->Float -> Point (Term t)
topLeft x y h w = floatP (left x w) (top y h)

floatP :: (PVal :<: t) => Float ->Float -> Point (Term t)
floatP x y = P (float x) (float y)

evaljumpToCorner ::  (ShapeDom :<: t,State :<: t,PVal :<: t) => ShapeCorner -> Term t ->  Term t -> Term t
evaljumpToCorner c s m =
     case evalState m of
        (St d p ) ->
         case (project s) of
           (Just (S p h w)) -> state d (findCorner c p h w )
           _ -> error "Type error: non-float values"

instance EvalM Move where
  evalAlgM Go    m = evalGo m
  evalAlgM (Change d')    m = evalChange d' m
  evalAlgM (JumpTo (P x y ))   m = evalJumpTo (P (x m) (y m) ) m
  evalAlgM (JumpToCorner c s)   m = evaljumpToCorner c (s m) m


instance EvalM Moves where
  evalAlgM (L []) m = m
  evalAlgM (L (x:xs)) m =
    case project (x m) of
        Just (St d p ) -> evalAlgM (L xs) (state d p)
