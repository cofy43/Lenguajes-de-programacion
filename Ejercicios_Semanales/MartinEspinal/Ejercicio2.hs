import Data.List
import Data.Char

type Identifier = String
type Substitucion = (Identifier, Expr)

data Expr = V Identifier
          | L Identifier Expr
          | App Expr Expr

instance Show Expr where
    show e = case e of 
        (V a) -> (show a)
        (L a b) -> "\\" ++ a ++ " -> " ++ (show b)
        (App a b) -> "(" ++ (show a) ++ " " ++ (show b) ++ ")"

frVars :: Expr -> [Identifier]
frVars (V id) = [id]
frVars (L id e) = (filter (/= id)) (frVars e)
frVars (App e1 e2) = union list1 list2
        where list1 = (frVars e1)
              list2 = (frVars e2)

incVar :: Identifier -> Identifier
incVar a
    | (isDigit lastChar) = (init a) ++ [intToDigit number]
    | otherwise =  a ++ ['1']
    where lastChar = (last a)
          number = ((digitToInt lastChar) + 1)

alphaExpr :: Expr -> Expr
alphaExpr (V a) = (V (incVar a))
alphaExpr (L id e) = (L (incVar id) (alphaExpr e))
alphaExpr (App e1 e2) = (App (alphaExpr e1) (alphaExpr e2))

subst :: Expr -> Substitucion -> Expr
subst (V a) (id, e)
    | (a == id) = e
    | otherwise = (V a)
subst (L a e) (id, e1) 
    | (a == id) = (L a e)
    | ((a /= id) && (notElem a (frVars e1))) = (L a (subst e (id, e1)))
    | otherwise = (subst alphaEquivalencia (id, e1))
    where alphaEquivalencia = (alphaExpr (L a e))
subst (App e1 e2) (id, e3) = (App (subst (e1) (id, e3)) (subst (e2) (id, e3)) )
--
beta :: Expr -> Expr
beta (V x) = (V x)
beta (L id e) = (L id (beta e))
beta (App (L id e) e1) = (subst e (id ,beta (e1)))
beta (App e1 e2) = (App (beta e1) (beta e2))

normal :: Expr -> Bool 
normal (V x) = True
normal (App (V x) (V y)) = False
normal (L id e) = (normal e)
normal (App e2 e1) = False

eval :: Expr -> Expr
eval (V x) = (V x)
eval (L id x) = (L id x)
eval (App (V x) (V y)) = (App (V x) (V y))
eval (App (L id e) e1) = (subst e (id ,eval (e1)))
eval (App e1 e2) = (App (eval e1) (eval e2))

--eval (App (L "n" (L "s" (L "z" (App (V "s") (App (App (V "n" ) (V "s" ) ) (V "z" ) ) ) ) ) ) (L "s" (L "z" (V "z" ) ) ) )