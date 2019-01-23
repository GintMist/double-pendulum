module Derivative where

import qualified Data.Map as M
import Control.Monad (sequence)
data Expr = Con String
          | Num Double
          | Fun String Int
          | Add [Expr]
          | Mul [Expr]
          | Div Expr Expr
          | Neg Expr
          | Cos Expr
          | Sin Expr
          | Ln Expr
          | Exp Expr
          | Pow Expr Double deriving (Eq, Ord)

evaluate :: Expr  -> M.Map Expr Double -> Maybe Double
evaluate (Num a) _ = Just a
evaluate (Con a) t = t M.!? Con a
evaluate (Fun a b) t = t M.!? Fun a b
evaluate (Add xs) t = fmap sum (traverse (`evaluate` t) xs)
evaluate (Mul xs) t = fmap product (traverse (`evaluate` t) xs)
evaluate (Div a b) t = (/) <$> evaluate a t <*> evaluate b t
evaluate (Neg a) t = negate <$> evaluate a t
evaluate (Cos a) t = cos <$> evaluate a t
evaluate (Sin a) t = sin <$> evaluate a t
evaluate (Ln a) t = (/(log $ exp 1)) <$> (log <$> evaluate a t)
evaluate (Exp a) t = exp <$> evaluate a t 
evaluate (Pow a b) t = (**b) <$> evaluate a t


derivative :: Expr -> Expr
derivative (Num _) = Num 0
derivative (Con _) = Num 0
derivative (Fun f o) = Fun f (o+1)
derivative (Add es) = Add (fmap derivative es)
derivative (Mul []) = Num 0
derivative (Mul (e:es)) = Add [Mul ((derivative e):es), Mul [derivative (Mul es),e]]
derivative (Div e1 e2) = Add [Mul [(derivative e1), e2], (Neg (Mul [e1, (derivative e2)]))]
derivative (Neg e) = Neg (derivative e)
derivative (Cos e) = Neg (Mul [(derivative e), (Sin e)])
derivative (Sin e) = Mul [(derivative e), (Cos e)]
derivative (Exp e) = Mul [(derivative e), (Exp e)]
derivative (Ln e) = Div (derivative e) e
derivative (Pow _ 0) = Num 0
derivative (Pow e n) = Mul [(Num n), (derivative e), (Pow e (n-1))]

partialDerivative :: Expr -> Expr -> Expr
partialDerivative (Num _) _ = Num 0
partialDerivative (Con _) _ = Num 0
partialDerivative (Fun f o) (Fun f2 o2) = if f == f2 && o ==o2 then Num 1 else Num 0
partialDerivative (Add es) f = Add (fmap ((flip partialDerivative) f)  es)
partialDerivative (Mul []) _ = Num 0
partialDerivative (Mul (e:es)) f = Add [Mul ((partialDerivative e f):es), Mul [partialDerivative (Mul es) f,e]]
partialDerivative (Div e1 e2) f = Add [Mul [(partialDerivative e1 f), e2], (Neg (Mul [e1, (partialDerivative e2 f)]))]
partialDerivative (Neg e) f = Neg (partialDerivative e f)
partialDerivative (Cos e) f = Neg (Mul [(partialDerivative e f), (Sin e)])
partialDerivative (Sin e) f = Mul [(partialDerivative e f), (Cos e)]
partialDerivative (Exp e) f = Mul [(partialDerivative e f), (Exp e)]
partialDerivative (Ln e) f = Div (partialDerivative e f) e
partialDerivative (Pow _ 0) _ = Num 0
partialDerivative (Pow e n) f = Mul [(Num n), (partialDerivative e f), (Pow e (n-1))]

simplify :: Expr -> Expr
simplify (Mul []) = Num 1
simplify (Mul es) = if elem (Num 0) es then Num 0 else Mul (fmap simplify es)
simplify (Add []) = Num 0
simplify (Add es) = Add $ fmap simplify (filter (/= (Num 0)) es)
simplify (Div (Num 0) _) = Num 0
simplify (Div e1 e2) = Div (simplify e1) (simplify e2)
simplify (Exp (Num 0)) = Num 1
simplify (Exp e) = Exp (simplify e)
simplify (Neg e) = Neg (simplify e)
simplify (Fun s o) = Fun s o
simplify (Con s) = Con s
simplify o = o 

instance Show Expr where
    show (Con s) = s
    show (Num f) = show f
    show (Fun s o) = s ++ (replicate o '\'')
    show (Add []     ) = show ""
    show (Add (e:[]) ) = show e
    show (Add (e:es) ) = (show e) ++ " + " ++ (show (Add es))
    show (Mul []    ) = show ""
    show (Mul (e:[])) = show e
    show (Mul (e:es)) = (show e) ++ "." ++ (show (Mul es))
    show (Div e1 e2) = "(" ++ show e1 ++ " / " ++ show e2 ++ ")"
    show (Neg e) = "-" ++ show e
    show (Cos e) = "cos(" ++ show e ++ ")"
    show (Sin e) = "sin(" ++ show e ++ ")" 
    show (Ln e) = "ln" ++ show e
    show (Exp e) = "e^("++show e++")"
    show (Pow e f) = show e ++ "^(" ++ show f++")"

