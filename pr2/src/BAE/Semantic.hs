module BAE.Semantic where

import BAE.Sintax

data Type = Integer | Boolean
type Decl = (Identifier , Type)
type TypCtxt = [Decl]

eval1 :: Expr -> Expr
eval1 (V x) = (V x)
eval1 (I n) = (I n)
eval1 (B True) = (B True)
eval1 (B False) = (B False)
eval1 (Add (I n) (I m)) = (I (n + m))
eval1 (Add (I n) m) = (Add (I n) (eval1 m))
eval1 (Add n m) = (Add (eval1 n) m)
eval1 (Mul (I n) (I m)) = (I (n * m))
eval1 (Mul (I n) m) = (Mul (I n) (eval1 m))
eval1 (Mul n m) = (Mul (eval1 n) m)
eval1 (Succ (I n)) = (I (n + 1))
eval1 (Succ e) = (Succ (eval1 e))
eval1 (Pred (I n)) = (I (n - 1))
eval1 (Pred e) = (Pred (eval1 e))
eval1 (Not (B False)) = (B True)
eval1 (Not (B True)) = (B False)
eval1 (Not e) = (Not (eval1 e))
eval1 (And (B True) (B True)) = (B True)
eval1 (And (B True) e) = (And (B True) (eval1 e))
eval1 (And _ _) = (B False)
eval1 (Or (B False) (B False)) = (B False)
eval1 (Or (B true) e) = (Or (B True) (eval1 e))
eval1 (Or _ _) = (B True)
eval1 (Lt (I m) (I n))
    | (n > m) = (I n)
    | otherwise = (I m)
eval1 (Lt (I n) e) = (Lt (I n) (eval1 e))
eval1 (Lt e1 e2) = (Lt (eval1 e1) e2)
eval1 (Gt (I n) (I m)) 
    | (n < m) = (I m)
    | otherwise = (I n)
eval1 (Gt (I n) e) = (Gt (I n) (eval1 e))
eval1 (Gt e1 e2) = (Gt (eval1 e1) e2)
eval1 (Eq (I n) (I m))
    | (n == m) = (B True)
    | otherwise = (B False)
eval1 (Eq (I n) e) = (Eq (I n) (eval1 e))
eval1 (Eq e1 e2) = (Eq (eval1 e1) e2)
eval1 (If (B True) e2 e3) = e2
eval1 (If (B False) e2 e3) = e3
eval1 (If e1 e2 e3) = (If (eval1 e1) (e2) (e3))
eval1 (Let x e1 e2) = (subst (e2) (x, (eval1 e1)))
eval1 (Fn id e) = (Fn id (eval1 e))
eval1 (App (Fn id e1) e2) = (subst e1 (id, e2))

evals :: Expr -> Expr
evals (I n) = (I n)
evals (B True) = (B True)
evals (B False) = (B False)
evals (V x) = (V x)
evals (Add (I n) (I m)) = (I (n + m))
evals (Add (I n) m) = evals (Add (I n) (evals m))
evals (Add n m) = evals (Add (evals n) (evals m))
evals (Mul (I n) (I m)) = (I (n * m))
evals (Mul (I n) m) = evals (Mul (I n) (evals m))
evals (Mul n m) = evals (Mul (evals n) (evals m))
evals (Succ (I n)) = (I (n + 1))
evals (Succ e) = evals (Succ (evals e))
evals (Pred (I n)) = (I (n - 1))
evals (Pred e) = evals (Pred (evals e))
evals (Not (B False)) = (B True)
evals (Not (B True)) = (B False)
evals (Not e) = evals (Not (evals e))
evals (And (B True) (B True)) = (B True)
evals (And (B True) e) = (And (B True) (evals e))
evals (And _ _) = (B False)
evals (Or (B False) (B False)) = (B False)
evals (Or (B true) e) = (Or (B True) (evals e))
evals (Or _ _) = (B True)
evals (Lt (I m) (I n))
    | (n > m) = (I n)
    | otherwise = (I m)
evals (Lt (I n) e) = evals (Lt (I n) (evals e))
evals (Lt e1 e2) = evals (Lt (evals e1) (evals e2))
evals (Gt (I n) (I m)) 
    | (n < m) = (I m)
    | otherwise = (I n)
evals (Gt (I n) e) = evals (Gt (I n) (evals e))
evals (Gt e1 e2) = evals (Gt (evals e1) (evals e2))
evals (Eq (I n) (I m))
    | (n == m) = (B True)
    | otherwise = (B False)
evals (Eq (I n) e) = evals (Eq (I n) (evals e))
evals (Eq e1 e2) = evals (Eq (evals e1) (evals e2))
evals (If (B True) e2 e3) = (evals e2)
evals (If (B False) e2 e3) = (evals e3)
evals (If e1 e2 e3) = evals (If (evals e1) (evals e2) (evals e3))
evals (Let x e1 e2) = (subst (e2) (x, (evals e1)))
evals (Fn id e) = (Fn id (evals e))
evals (App (V x) (V y)) = (App (V x) (V y))
evals (App (Fn id e1) e2) = evals (subst e1 (id, (evals e2)))
evals (App (V x) e2) = (App (V x) (evals e2))
evals (App e1 (V x)) = (App (evals e1) (V x))
evals (App e1 e2) = (evals (App (evals e1) (evals e2)))

evale :: Expr -> Expr
evale (V x) = (V x)
evale (I n) = (I n)
evale (B True) = (B True)
evale (B False) = (B False)
evale (Add (I n) (I m)) = (I (n + m))
evale (Add (I n) (B b)) = error "Exception: [Add] Expects two Integer"
evale (Add (B b) (I n)) = error "Exception: [Add] Expects two Integer"
evale (Add (B b) (B n)) = error "Exception: [Add] Expects two Integer"
evale (Add (I n) m) = (evale (Add (I n) (evale m)))
evale (Add n m) = (evale (Add (evale n) m))
evale (Mul (I n) (I m)) = (I (n * m))
evale (Mul (I n) (B b)) = error "Exception: [Mul] Expects two Integer"
evale (Mul (B b) (I n)) = error "Exception: [Mul] Expects two Integer"
evale (Mul (B b) (B n)) = error "Exception: [Mul] Expects two Integer"
evale (Mul (I n) m) = (evale (Mul (I n) (evale m)))
evale (Mul n m) = (evale (Mul (evale n) m))
evale (Succ (I n)) = (I (n + 1))
evale (Succ (B b)) = error "Exception: [Succ] Expects one Integer"
evale (Succ e) = (evale (Succ (evale (e))))
evale (Pred (I n)) = (I (n - 1))
evale (Pred (B b)) = error "Exception: [Pred] Expects one Integer"
evale (Pred e) = (evale (Pred (evale (e))))
evale (Not (B True)) = (B False)
evale (Not (B False)) = (B True)
evale (Not (I n)) = error "Exception: [Not] Expects Boolean"
evale (Not e) = (evale (Not (evale e)))
evale (And (B True) (B True)) = (B True)
evale (And (B True) (B False)) = (B False)
evale (And (B False) (B True)) = (B False)
evale (And (B False) (B False)) = (B False)
evale (And (I n) (B e)) = error "Exception: [And] Expects two Boleans"
evale (And (B e) (I n)) = error "Exception: [And] Expects two Boleans"
evale (Or (B True) (B True)) = (B True)
evale (Or (B True) (B False)) = (B True)
evale (Or (B False) (B True)) = (B True)
evale (Or (B False) (B False)) = (B False)
evale (Or (I n) (B e)) = error "Exception: [Or] Expects two Boleans"
evale (Or (B e) (I n)) = error "Exception: [Or] Expects two Boleans"
evale (Lt (I n) (I m)) 
    | (n < m) = (I n)
    | otherwise = (I m)
evale (Lt (I n) (B e)) = error "Exception: [Lt] Expects two Integers"
evale (Lt (B e) (I n)) = error "Exception: [Lt] Expects two Integers"
evale (Gt (I n) (I m)) 
    | (n > m) = (I n)
    | otherwise = (I m)
evale (Gt (I n) (B e)) = error "Exception: [Gt] Expects two Integers"
evale (Gt (B e) (I n)) = error "Exception: [Gt] Expects two Integers"
evale (Eq (I n) (I m)) 
    | (n == m) = (B True)
    | otherwise = (B False)
evale (Eq (I n) (B e)) = error "Exception: [Eq] Expects two Integers"
evale (Eq (B e) (I n)) = error "Exception: [Eq] Expects two Integers"
evale (If (B True) e1 e2) = (evals e1)
evale (If (B False) e1 e2) = (evals e2)
evale (If (I n) e1 e2) = error "Exception: [If] Expects boolean and two Expt"
evale (Let x e1 e2) = evale (subst e2 (x, (e1)))
evale (Fn id e) = (Fn id (evals e))
evale (App (Fn id e1) e2) = evals (subst e1 (id, (evals e2)))
evale (App _ e) = error "Exception: [App] Expects a Funtion as firts argument"
evale (App e1 e2) = (evals (App (evals e1) (evals e2)))

eqDecl :: Decl -> Decl -> Bool
eqDecl (x, Boolean) (y, Boolean) = (x == y)
eqDecl (x, Integer) (y, Integer) = (x == y)
eqDecl _ _ = False

find :: [Decl] -> Decl -> Bool
find [] _ = False
find (d:ds) x | (eqDecl d x) = True
            | otherwise = find ds x

vt :: TypCtxt -> Expr -> Type -> Bool
vt _ (B p) Boolean = True
vt _ (B p) Integer = False
vt _ (I m) Integer = True
vt _ (I m) Boolean = False
vt ctx (V x) t = find ctx (x, t)
vt ctx (Add e1 e2) Integer = (vt ctx e1 Integer) && (vt ctx e2 Integer)
vt ctx (Add e1 e2) Boolean = False
vt ctx (If e1 e2 e3) t = ((vt ctx e1 Boolean) && (vt ctx e2 t) && (vt ctx e3 t))
vt ctx (Mul e1 e2) Integer = (vt ctx e1 Integer) && (vt ctx e2 Integer)
vt ctx (Mul e1 e2) Boolean = False
vt ctx (Succ e1) Integer = (vt ctx e1 Integer)
vt ctx (Succ e1) Boolean = False
vt ctx (Pred e1) Integer = (vt ctx e1 Integer)
vt ctx (Pred e1) Boolean = False
vt ctx (Not e1) Boolean = (vt ctx e1 Boolean)
vt ctx (Not e1) Integer = False
vt ctx (And e1 e2) Boolean = (vt ctx e1 Boolean) && (vt ctx e2 Boolean)
vt ctx (And e1 e2) Integer = False
vt ctx (Or e1 e2) Boolean = (vt ctx e1 Boolean) && (vt ctx e2 Boolean)
vt ctx (Or e1 e2) Integer = False
vt ctx (Lt e1 e2) Boolean = (vt ctx e1 Integer) && (vt ctx e2 Integer)
vt ctx (Lt e1 e2) Integer = False
vt ctx (Gt e1 e2) Boolean = (vt ctx e1 Integer) && (vt ctx e2 Integer)
vt ctx (Gt e1 e2) Integer = False
vt ctx (Eq e1 e2) Boolean = (vt ctx e1 Integer) && (vt ctx e2 Integer)
vt ctx (Eq e1 e2) Integer = False
vt ctx (Let x e1 e2) t 
    | (vt ctx e1 Integer) = (vt (ctx ++ [(x, Integer)]) e2 t)
    | (vt ctx e1 Boolean) = (vt (ctx ++ [(x, Boolean)]) e2 t)

eval :: Expr -> Type -> Expr
eval e t 
    | (vt [] e t) = evals e
    | otherwise = error "Type error"
