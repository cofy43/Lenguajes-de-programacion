module BAE.Sintax where 

import Data.List
import Data.Char

type Identifier = String
type Subtitution = (Identifier, Expr)

data Expr = V Identifier
          | I Int 
          | B Bool
          | Add Expr Expr
          | Mul Expr Expr
          | Succ Expr
          | Pred Expr
          | Not Expr
          | And Expr Expr
          | Or Expr Expr
          | Lt Expr Expr
          | Gt Expr Expr
          | Eq Expr Expr
          | If Expr Expr Expr
          | Let Identifier Expr Expr 
          | Fn Identifier Expr
          | App Expr Expr deriving (Eq)

instance Show Expr where
    show e = case e of
        (V x) -> "V[" ++ (show x) ++ "]"
        (I n) -> "N[" ++ (show n) ++ "]"
        (B b) -> "B[" ++ (show b) ++ "]"
        (Add a b) -> "ADD[" ++ (show a) ++ " + " ++ (show b) ++ "]"
        (Mul a b) -> "MUL[" ++ (show a) ++ " * " ++ (show b) ++ "]"
        (Succ n) -> "SUCC[" ++  (show n) ++ "]"
        (Pred n) -> "PRED[" ++ (show n) ++  "]"
        (Not n) -> "NOT[" ++ (show n) ++ "]"
        (And a b) -> "ADN[" ++ (show a) ++ " && " ++ (show b) ++ "]"
        (Or a b) -> "OR[" ++ (show a) ++ " || " ++ (show b) ++ "]" 
        (Lt a b) -> "LT[" ++ (show a) ++ " < " ++ (show b) ++ "]"
        (Gt a b) -> "Gt[" ++ (show a) ++ " > " ++ (show b) ++ "]"
        (Eq a b) -> "EQ[" ++ (show a) ++ " = " ++ (show b) ++ "]"
        (If a b c) -> "IF[" ++ (show a) ++ " then " ++ (show b) ++ " else " ++ (show c) ++ "]"
        (Let x c1 c2) -> "Let[" ++ (show c1) ++ ", " ++ x ++ "." ++ (show c2) ++ "]"
        (Fn x e) -> "fn(" ++ x ++ "." ++ (show e) ++ ")"
        (App e1 e2) -> "app(" ++ (show e1) ++ ", " ++ (show e2) ++ ")"

frVars :: Expr -> [Expr]
frVars e = case e of
    (V x) -> [(V x)]
    (I n) -> []
    (B b) -> []
    (Add a b) -> union (frVars a) (frVars b)
    (Mul a b) -> union (frVars a) (frVars b)
    (And a b) -> union (frVars a) (frVars b)
    (Or a b) -> union (frVars a) (frVars b)
    (Lt a b) -> union (frVars a) (frVars b)
    (Gt a b) -> union (frVars a) (frVars b)
    (Eq a b) -> union (frVars a) (frVars b)
    (Pred n) -> (frVars n)
    (Succ n) -> (frVars n)
    (Not n) -> (frVars n)
    (If a b c) -> union (frVars a) (union (frVars b) (frVars c))
    (Let x c1 c2) -> union (frVars c1) (filter (/= (V x)) (frVars c2))
    (Fn x e) -> (filter (/= (V x)) (frVars e))
    (App e1 e2) -> union (frVars e1) (frVars e2)

incVar :: Identifier -> Identifier
incVar a
--La funcion isDigit nos indica si un caracter es un numero
--La funcion intToDigit convierte un numero a un caracter
    | (isDigit lastChar) = (init a) ++ [intToDigit number]
    | otherwise =  a ++ ['1']
    where lastChar = (last a)
          number = ((digitToInt lastChar) + 1)

subst :: Expr -> Subtitution -> Expr
subst e s@(y, es) = case e of
    (V x) -> if (x == y) then es else e
    (I n) -> e
    (B b) -> e
    (Add a b) -> (Add (subst a s) (subst b s))
    (Mul a b) -> (Mul (subst a s) (subst b s))
    (And a b) -> (And (subst a s) (subst b s))
    (Or a b) -> (Or (subst a s) (subst b s))
    (Lt a b) -> (Lt (subst a s) (subst b s))
    (Gt a b) -> (Gt (subst a s) (subst b s))
    (Eq a b) -> (Eq (subst a s) (subst b s))
    (Pred n) -> (Pred (subst n s))
    (Succ n) -> (Succ (subst n s))
    (Not n) -> (Not (subst n s))
    (If a b c) -> (If (subst a s) (subst b s) (subst c s))
    (Let x c1 c2) -> if (y == x) then
                        (Let x (subst c1 s) c2)
                    else if ((V x) `elem` (frVars c1)) then 
                        error "NO SE PUEDE REALIZAR LA SUBSTITUCION"
                    else 
                        (Let x (subst c1 s) (subst c2 s))
    (Fn a e) -> if (a == y) then
                    (Fn a e)
                else if ((a /= y) && (notElem (V a) (frVars es))) then
                        (Fn a (subst e s))
                else if ((a /= y) && (elem (V a) (frVars es))) then
                        (Fn (incVar a) (subst e s))
                else (Fn a e)
    (App e1 e2) -> (App (subst e1 s) (subst e2 s))


alphaEq :: Expr -> Expr -> Bool
alphaEq (V x) (V y) = (x == y)
alphaEq (I n) (I m) = (n == m)
alphaEq (B b1) (B b2) = (b1 == b2)
alphaEq (Add e1 e2) (Add c1 c2) = (alphaEq e1 c1) && (alphaEq e2 c2)
alphaEq (Mul e1 e2) (Mul c1 c2) = (alphaEq e1 c1) && (alphaEq e2 c2)
alphaEq (Or e1 e2) (Or c1 c2) = (alphaEq e1 c1) && (alphaEq e2 c2)
alphaEq (And e1 e2) (And c1 c2) = (alphaEq e1 c1) && (alphaEq e2 c2)
alphaEq (Lt e1 e2) (Lt c1 c2) = (alphaEq e1 c1) && (alphaEq e2 c2)
alphaEq (Gt e1 e2) (Gt c1 c2) = (alphaEq e1 c1) && (alphaEq e2 c2)
alphaEq (Eq e1 e2) (Eq c1 c2) = (alphaEq e1 c1) && (alphaEq e2 c2)
alphaEq (Not e1) (Not e2) = (alphaEq e1 e2)
alphaEq (Succ e1) (Succ e2) = (alphaEq e1 e2)
alphaEq (Pred e1) (Pred e2) = (alphaEq e1 e2)
alphaEq (If b e1 e2) (If c c1 c2) = (alphaEq e1 c1) && (alphaEq e2 c2) && (alphaEq b c)
alphaEq (Let x e1 e2) (Let y c1 c2) = (alphaEq e1 c1) && (alphaEq c2 (subst e2 (x, V y)))
alphaEq _ _ = False

alphaExpr :: Expr -> Expr
alphaExpr (V a) = (V (incVar a))
alphaExpr (Fn id e) = (Fn (incVar id) (alphaExpr e))
alphaExpr (App e1 e2) = (App (alphaExpr e1) (alphaExpr e2))