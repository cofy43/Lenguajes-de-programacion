module BAE.Semantic where

import Data.List
import Data.Maybe
import BAE.Sintax as Sintax
import BAE.Memory as Memory

data Type = Integer | Boolean
type Decl = (Identifier , Type)
type TypCtxt = [Decl]
--eval1 (R ( [ ] , [OrFR (B True) ( ) , AndFL ( ) (B False ) ] ,B False ) )
--eval1 (E ( [ ] , [ ] , Add ( I 1 ) ( I 2 ) ) )E ( [ AddFL ( ) ( I 2 ) ] , I 1 )
--eval1 :: (Memory , Expr) -> (Memory , Expr)
--eval1 (m, (Void)) = (m, Void)
--eval1 (m, (L n)) = (m, (L n))
--eval1 (m, (Seq Void e2)) = (m, e2)
--eval1 (m, (Seq e1 e2)) = (m, (Seq (snd (eval1 (m, e1))) e2))
--eval1 (m, (While (B True) e2)) = (m, (If (B True) (Seq e2 (While (B True) e2)) (Void)))
--eval1 (m, (While (B False) e2)) = (m, (If (B False) (Seq e2 (While (B False) e2)) (Void)))
--eval1 (m, (While e1 e2)) = (m, (While (snd (eval1 (m, e1))) e2))
--eval1 (m, (Alloc e1)) = (m, (Alloc (snd (eval1 (m, e1)))))
--eval1 (m, (Deref e1)) = (m, (Deref (snd (eval1 (m, e1)))))
--eval1 (m, (Deref (L n))) = (m, (inValue (access  n m)))
--eval1 (m, (Assig (L n) (V x))) = ((inMemory $ update (n, (V x)) m), Void)
--eval1 (m, (Assig (L n) (I x))) = ((inMemory $ update (n, (I x)) m), Void)
--eval1 (m, (Assig (L n) (B b))) = ((inMemory $ update (n, (B b)) m), Void)
--eval1 (m, (Assig (L n) (Fn id e))) = ((inMemory $ update (n, (Fn id e)) m), Void)
--eval1 (m, (Assig (L n) Void)) = ((inMemory $ update (n, Void) m), Void)
--eval1 (m, (Assig e1 e2)) = (m, (Assig (snd (eval1 (m, e1))) e2))
--eval1 (m, (Assig (L n) e2)) = (m, (Assig (L n)) (snd (eval1 (m, e2))))
--eval1 (m, (V x)) = (m, (V x))
--eval1 (m, (I n)) = (m, (I n))
--eval1 (m, (B True)) = (m, (B True))
--eval1 (m, (B False)) = (m, (B False))
--eval1 (mem, (Add (I n) (I m))) = (mem, (I (n + m)))
--eval1 (mem, (Add (I n) m)) = (mem, (Add (I n) (snd (eval1 (mem, m)))))
--eval1 (mem, (Add n m)) = (mem, (Add (snd (eval1 (mem, n))) m))
--eval1 (mem, (Mul (I n) (I m))) = (mem, (I (n * m)))
--eval1 (mem, (Mul (I n) m)) = (mem, (Mul (I n) (snd (eval1 (mem, m)))))
--eval1 (mem, (Mul n m)) = (mem, (Mul (snd (eval1 (mem, n))) m))
--eval1 (m, (Succ (I n))) = (m, (I (n + 1)))
--eval1 (m, (Succ e)) = (m, (Succ (snd (eval1 (m, e)))))
--eval1 (m, (Pred (I n))) = (m, (I (n - 1)))
--eval1 (m, (Pred e)) = (m, (Pred (snd (eval1 (m, e)))))
--eval1 (m, (Not (B False))) = (m, (B True))
--eval1 (m, (Not (B True))) = (m, (B False))
--eval1 (m, (Not e)) = (m,(Not (snd (eval1 (m, e)))))
--eval1 (m, (And (B True) (B True))) = (m, (B True))
--eval1 (m, (And (B True) e)) = (m, (And (B True) (snd (eval1 (m, e)))))
--eval1 (m, (And _ _)) = (m, (B False))
--eval1 (m, (Or (B False) (B False))) = (m, (B False))
--eval1 (m, (Or (B true) e)) = (m, (Or (B True) (snd (eval1 (m, e)))))
--eval1 (m, (Or _ _)) = (m, (B True))
--eval1 (mem, (Lt (I m) (I n)))
-- | (n > m) = (mem, (I n))
-- | otherwise = (mem, (I m))
--eval1 (m, (Lt (I n) e)) = (m, (Lt (I n) (snd (eval1 (m, e)))))
--eval1 (m, (Lt e1 e2)) = (m, (Lt (snd (eval1 (m, e1))) e2))
--eval1 (mem, (Gt (I n) (I m)))
-- | (n < m) = (mem, (I m))
-- | otherwise = (mem, (I n))
--eval1 (m, (Gt (I n) e)) = (m, (Gt (I n) (snd (eval1 (m, e)))))
--eval1 (m, (Gt e1 e2)) = (m, (Gt (snd (eval1 (m, e1))) e2))
--eval1 (mem, (Eq (I n) (I m)))
-- | (n == m) = (mem, (B True))
-- | otherwise = (mem, (B False))
--eval1 (m, (Eq (I n) e)) = (m, (Eq (I n) (snd (eval1 (m, e)))))
--eval1 (m, (Eq e1 e2)) = (m, (Eq (snd (eval1 (m, e1))) e2))
--eval1 (m, (If (B True) e2 e3)) = (m, e2)
--eval1 (m, (If (B False) e2 e3)) = (m, e3)
--eval1 (m, (If e1 e2 e3)) = (m, (If (snd (eval1 (m, e1))) (e2) (e3)))
--eval1 (m, (Let x e1 e2)) = (m, (subst (e2) (x, (snd (eval1 (m, e1))))))
--eval1 (m, (Fn id e)) = (m, (Fn id (snd (eval1 (m, e)))))
--eval1 (m, (App (Fn id e1) e2)) = (m, (subst e1 (id, e2)))

--eval1 :: State -> State
--eval1 (E (m, s, e))
-- | (isValue e) = (R (m, s, e))
-- | otherwise = case e of 
--    (Add e1 e2) -> (E (m, s:(AddFL () e2), e1))
--    (Add (I n) e2) -> (E (m, s:(AddFR e2 ()), e2))

{- mapUnaryToPrimitive :: Expr -> (a -> b)
mapUnaryToPrimitive e = case e of
    (Succ e) -> (\x -> x + 1)
    (Pred e) -> (\x -> if x == 0 then 0 else pred x)
    (Not e) -> (\x -> not x)
    (Deref e) -> True
    (Alloc e) -> True
    _ -> False

isUnary :: Expr -> Bool
isUnary e = case e of
    (Succ e) -> True
    (Pred e) -> True
    (Not e) -> True
    (Deref e) -> True
    (Alloc e) -> True
    _ -> False

evalUnary :: State -}

addressFromCell :: Expr -> Address
addressFromCell (L n) = n 
addressFromCell e = error "addressFromCell expects a ref"

eval1 :: State -> State
eval1 f = case f of
    -- (E (m, s, (LetCC id e1))) -> (E (m, s, subst e1 (id, Cont s)))
    -- (E (m, s, (Continue e1 e2))) -> (E (m, s, (ContinueFL)))
    (E (m, s, e)) -> if (isValue e) then (R (m, s, e)) else 
        case e of
            (Add e1 e2) -> (E (m, (AddFL () e2):s, e1))
            (Mul e1 e2) -> (E (m, (MulFL () e2):s, e1))
            (Or e1 e2) -> (E (m, (OrFL () e2):s, e1))
            (Lt e1 e2) -> (E (m, (LtFL () e2):s, e1))
            (Gt e1 e2) -> (E (m, (GtFL () e2):s, e1))
            (And e1 e2) -> (E (m, (AndFL () e2):s, e1))
            (Seq e1 e2) -> (E (m, (SeqFL () e2):s, e1))
            (Assig e1 e2) -> (E (m, (AssigFL () e2):s, e1))
            (App e1 e2) -> (E (m, (AppFL () e2):s, e1))
            (While g e1) -> (E (m, (WhileF () e1):s, g))
            (If g e1 e2) -> (E (m, (IfF () e1 e2):s, g))
            (Let id e1 e2) -> (E (m, (LetF id () e2):s, e1))
            (Fn id e) -> (E (m, (FnF id ()):s, e))
            (Succ e1) -> (E (m, (SuccF ()):s, e1))
            (Pred e1) -> (E (m, (PredF ()):s, e1))
            (Alloc e1) -> (E (m, (AllocF ()):s, e1))
            (Not e1) -> (E (m, (NotF ()):s, e1))
            (Deref e1) -> (E (m, (DerefF ()):s, e1))
            (Raise e1) -> (E (m, (RaiseF ()):s, e1))
            (Handle e1 id e2) -> (E (m, (HandleF () id e2):s, e1))
            (LetCC id e1) -> (E (m, s, subst e1 (id, Cont s)))
            (Continue e1 e2) -> (E (m, (ContinueFL () e2):s, e1))
    (R (m, (AddFL p e2):xs, v)) -> (E (m, (AddFR v ()):xs, e2))
    (R (m, (AddFR (I n1) p):xs, (I n2))) -> (R (m, xs, (I (n1 + n2))))
    (R (m, (AddFR (I n1) p):xs, v)) -> (P (m, xs, Raise (v)))
    (R (m, (MulFL p e2):xs, v)) -> (E (m, (MulFR v ()):xs, e2))
    (R (m, (MulFR (I n1) p):xs, (I n2))) -> (R (m, xs, (I (n1 * n2))))
    (R (m, (MulFR (I n1) p):xs, v)) -> (P (m, xs, v))
    (R (m, (OrFL p e2):xs, v1)) -> (E (m, (OrFR v1 ()):xs, e2))
    (R (m, (OrFR (B q) p):xs, (B r))) -> (R (m, xs, (B (q || r))))
    (R (m, (OrFR v p):xs, (B r))) -> (P (m, xs, Raise v))
    (R (m, (OrFR (B q) p):xs, v)) -> (P (m, xs, Raise v))
    (R (m, (LtFL p e2):xs, v1)) -> (E (m, (LtFR v1 ()):xs, e2))
    (R (m, (LtFR (I n1) p):xs, (I n2))) -> (R (m, xs, (B (n1 < n2))))
    (R (m, (LtFR v p):xs, (I n2))) -> (P (m, xs, v))
    (R (m, (LtFR (I n1) p):xs, v)) -> (P (m, xs, v))
    (R (m, (GtFL p e2):xs, v1)) -> (E (m, (GtFR v1 ()):xs, e2))
    (R (m, (GtFR (I n1) p):xs, (I n2))) -> (R (m, xs, (B (n1 > n2))))
    (R (m, (AndFL p e2):xs, v1)) -> (E (m, (AndFR v1 ()):xs, e2))
    (R (m, (AndFR (B q) p):xs, (B r))) -> (R (m, xs, (B (q && r))))
    (R (m, (AndFR v p):xs, (B r))) -> (P (m, xs, Raise v))
    (R (m, (AndFR (B q) p):xs, v)) -> (R (m, xs, v))
    (R (m, (NotF p):xs, (B q))) -> (R (m, xs, (B (not q))))
    (R (m, (NotF p):xs, v)) -> (P (m, xs, v))
    (R (m, (PredF p):xs, (I n))) -> (R (m, xs, (I (if n == 0 then 0 else n - 1))))
    (R (m, (PredF p):xs, v)) -> (P( m, xs, Raise v))
    (R (m, (SuccF p):xs, (I n))) -> (R (m, xs, (I (n + 1))))
    (R (m, (SuccF p):xs, v)) -> (P (m, xs, Raise v))
    (R (m, (DerefF p):xs, (L a))) -> (R (m, xs, value)) where value = extractValue (access a m)
    (R (m, (DerefF p):xs, v)) -> (P (m, xs, Raise v))
    (R (m, (AllocF p):xs, v)) -> (R ((address, v):m, xs, (L address))) where address = (addressFromCell ( newAddress m))
    (R (m, (SeqFL p e2):xs, v1)) -> (E (m, (SeqFR v1 ()):xs, e2))
    (R (m, (SeqFR Void p):xs, v)) -> (R (m, xs, Raise v))
    (R (m, (SeqFR v1 p):xs, v)) -> (P (m ,xs, Raise v1))
    (R (m, (AssigFL p e2):xs, v)) -> (E (m, (AssigFR v ()):xs, e2))
    (R (m, (AssigFR (L n) p):xs, v)) -> (R (m2, xs, Void)) where m2 = (fromMaybe [] (update (n, v) m))
    (R (m, (AssigFR v1 p):xs, v)) -> (P (m, xs, Raise v))
    (R (m, (LetF id p e1):xs, v)) -> (E (m, xs, subst e1 (id, v)))
    (R (m, (AppFL p e2):xs, v)) -> (E (m, (AppFR v ()):xs, e2))
    (R (m, (AppFR (Fn id e3) p):xs, v)) -> (E (m, xs, subst e3 (id, v)))
    (R (m, (AppFR v1 p):xs, v)) -> (P (m, xs, v1))
    (R (m, (IfF p e1 e2):xs, (B q))) -> if (q) then (E (m, xs, e1)) else (E (m, xs, e2))
    (R (m, (IfF p e1 e2):xs, v)) -> (P (m, xs, Raise v))
    (R (m, (RaiseF p):xs, v)) -> (P (m, xs, v))
    (P (m, (HandleF p id e1):xs, (Raise e2))) -> (E (m, xs, subst e1 (id, e2)))
    (R (m, (HandleF p id e2):xs, v)) -> (R (m, xs, v))
    (P (m, f:xs, (Raise e1))) -> (P (m, xs, (Raise e1)))
    (R (m, (ContinueFL p e2):xs, v)) -> (E (m, (ContinueFR v ()):xs, e2))
    (R (m, (ContinueFR (Cont s2) p):xs, v)) -> (R (m, s2,v))
    (R (m, (ContinueFR v1 p):xs, v)) -> (P (m, xs, Raise v1))

extractValue :: Maybe Value -> Value
extractValue (Nothing) = error "Cannot extract value from Nothing"
extractValue (Just v) = v

extarctExpr :: State -> Expr
extarctExpr (E (m, s, e)) = e
extarctExpr (R (m, s, e)) = e
extarctExpr (P (m, s, e)) = e

extarctChar :: Expr -> String
extarctChar (V x) = x
extarctChar _ = error "No se evaluo a un string"

isValue :: Expr -> Bool
isValue x = case x of
    (I n) -> True
    (B b) -> True
    (V id) -> True

    (Fn id e) -> True
    (L c) -> True
    (Void) -> True
    _ -> False

evals :: State -> State
evals f = case f of
    (E (m, s, e)) -> if (isValue e) then (R (m, s, e)) else 
        case e of
            (Void) -> (R (m, s, Void))
            (Add (I n) (I l)) -> (R (m, s, I (l+n)))
            (Add (I n) e2) -> evals (E (m, s, (Add (I n) (extarctExpr(evals(E (m,s,e2)))))))
            (Add e1 e2) -> evals (E (m, s, (Add (extarctExpr(evals (E (m, s, e1)))) e2)))
            (Mul (I n) (I l)) -> (R (m, s, I (n*l)))
            (Mul (I n) e1) -> evals (E (m, s, (Mul (I n) (extarctExpr(evals (E (m, s, e1)))))))
            (Mul e1 e2) -> evals (E (m, s, (Mul (extarctExpr(evals (E (m, s, e1)))) e2)))
            (Or (B True) (B _)) -> (R (m, s, B (True)))
            (Or (B _) (B True)) -> (R (m, s, B (True)))
            (Or (B _) (B _)) -> (R (m, s, B (False)))
            (Or (B a) e2) -> evals (E (m, s, (Or (B a) (extarctExpr(evals (E (m, s, e2)))))))
            (Or e1 e2) -> evals (E (m, s, (Or (extarctExpr (evals (E (m, s, e1)))) e2)))
            (Lt (I n) (I l)) -> if (n > l) then (R (m, s, (I n))) else (R (m, s, (I l)))
            (Lt (I n) e2) -> evals (E (m, s, (Lt (I n) (extarctExpr (evals (E (m, s, e2)))))))
            (Lt e1 e2) -> evals (E (m, s, (Lt (extarctExpr (evals (E (m, s, e1)))) e2)))
            (Gt (I n) (I l)) -> if (n < l) then (R (m, s, (I n))) else (R (m, s, (I l)))
            (Gt (I n) e2) -> evals (E (m, s, (Gt (I n) (extarctExpr (evals (E (m, s, e2)))))))
            (Gt e1 e2) -> evals (E (m, s, (Gt (extarctExpr (evals (E (m, s, e1)))) e2)))
            (Eq (I n) (I l)) -> (R (m, s, B (n == l)))
            (Eq (I n) e2) -> evals (E (m, s, (Eq (I n) (extarctExpr(evals (E (m, s, e2)))))))
            (Eq e1 e2) -> evals (E (m, s, (Eq (extarctExpr(evals (E (m, s, e1)))) e2)))
            (And (B True) (B True)) -> (R (m, s, (B True)))
            (And (B b) (B a)) -> (R (m, s, (B False)))
            (And (B b) e2) -> evals (E (m, s, (And (B b)) (extarctExpr (evals (E (m, s, e2))))))
            (And e1 e2) -> evals (E (m, s, Add (extarctExpr (evals (E (m, s, e1)))) e2))
            (Seq Void l) -> evals (E (m, s, l))
            (Seq e1 e2) -> evals (E (m, s, Seq (extarctExpr (evals (E (m, s, e1)))) e2))
            (Assig (L n) (V x)) -> evals (E ((inMemory $ update (n, (V x)) m), s, Void))
            (Assig (L n) (B b)) -> evals (E ((inMemory $ update (n, (B b)) m), s, Void))
            (Assig (L n) (I x)) -> evals (E ((inMemory $ update (n, (I x)) m), s, Void))
            (Assig (L n) (Fn id e)) -> evals (E ((inMemory $ update (n, (Fn id e)) m), s, Void))
            (Assig e1 e2) -> evals (E (m, s, (Assig (extarctExpr(evals (E (m, s, e1)))) e2)))
            (App (V x) (V y)) -> (R (m, s, (App )(V x) (V y)))
            (App (Fn id e1) e2) -> evals (E (m, s, (subst (e1) (id, e2))))
            (App e1 e2) -> evals (E (m, s, (App (extarctExpr (evals (E (m, s, e1)))) e2)))
            (While g e1) -> evals (E (m, s, (evaluacion))) where evaluacion = extarctExpr $ evals (E (m, s, ciclo))
                                                                 ciclo = extarctExpr $ evals (E (m, s, (If (extarctExpr $ evals (E (m, s, g))) (Seq (e1) (While g e1)) (Void))))
            (If (B True) e1 e2) -> evals (E (m, s, extarctExpr $ evals (E (m, s, e1))))
            (If (B False) e1 e2) -> evals (E (m, s, extarctExpr $ evals (E (m, s, e2))))
            (If g e1 e2) -> evals (E (m, s, (If (extarctExpr $ evals (E (m, s, g))) e1 e2)))
            (Let id e1 e2) -> evals (E (m, s, (subst (e2) (id, e1))))
            --(Let e1 e2 e3) -> evals (E (m, s, (Let id e2 e3))) where id = extarctChar $ extarctExpr $ evals (E (m, s, e1))
            (Fn id e) -> (R (m, s, (Fn id e)))
            (Succ (I n)) -> (R (m, s, I (n+1)))
            (Succ e1) -> evals (E (m, s, (Succ (extarctExpr $ evals (E (m, s, e1))))))
            (Pred (I n)) -> (R (m, s, I (n-1)))
            (Pred e1) -> evals (E (m, s, Pred (extarctExpr $ evals (E (m, s, e1)))))
            (Alloc e1) -> evals (E (m, s, Alloc (extarctExpr $ evals (E (m, s, e1)))))
            (Not (B True)) -> (R (m, s, (B False)))
            (Not (B False)) -> (R (m, s, (B True)))
            (Not e1) -> (E (m, s, Not (extarctExpr $ evals (E (m, s, e1)))))
            (Deref (L n)) -> evals (E (m, s, extarctExpr $ evals (E (m, s, inValue(access n m)))))
            (Deref e1) -> evals (E (m, s, Deref (extarctExpr $ evals(E (m, s, e1)))))
            (Raise e1) -> evals (E (m, s, Raise $ extarctExpr $ evals (E (m, s, e1))))
            (Handle e1 id e2) -> evals (E (m, s, (Handle (extarctExpr $ evals(E (m, s, e1))) id e2)))
            (LetCC id e1) -> evals (E (m, s, subst e1 (id, Cont s)))
            (Continue e1 e2) -> (E (m, s, (Continue (extarctExpr $ evals (E (m, s, e1))) e2) ))
    --(R (m, (AddFL p e2):xs, v)) -> (E (m, (AddFR v ()):xs, e2))
    --(R (m, s, (Add (I n1) (I n2)))) -> (R (m, s, (I (n1 + n2))))
    --(R (m, (MulFL p e2):xs, v)) -> (E (m, (MulFR v ()):xs, e2))
    --(R (m, (MulFR (I n1) p):xs, (I n2))) -> (R (m, xs, (I (n1 * n2))))
    --(R (m, (OrFL p e2):xs, v1)) -> (E (m, (OrFR v1 ()):xs, e2))
    --(R (m, (OrFR (B q) p):xs, (B r))) -> (R (m, xs, (B (q || r))))
    --(R (m, (LtFL p e2):xs, v1)) -> (E (m, (LtFR v1 ()):xs, e2))
    --(R (m, (LtFR (I n1) p):xs, (I n2))) -> (R (m, xs, (B (n1 < n2))))
    --(R (m, (GtFL p e2):xs, v1)) -> (E (m, (GtFR v1 ()):xs, e2))
    --(R (m, (GtFR (I n1) p):xs, (I n2))) -> (R (m, xs, (B (n1 > n2))))
    --(R (m, (AndFL p e2):xs, v1)) -> (E (m, (AndFR v1 ()):xs, e2))
    --(R (m, (AndFR (B q) p):xs, (B r))) -> (R (m, xs, (B (q && r))))
    --(R (m, (NotF p):xs, (B q))) -> (R (m, xs, (B (not q))))
    --(R (m, (PredF p):xs, (I n))) -> (R (m, xs, (I (if n == 0 then 0 else n - 1))))
    --(R (m, (SuccF p):xs, (I n))) -> (R (m, xs, (I (n + 1))))
    --(R (m, (DerefF p):xs, (L a))) -> (R (m, xs, value)) where value = extractValue (access a m)
    --(R (m, (AllocF p):xs, v)) -> (R ((address, v):m, xs, (L address))) where address = (addressFromCell ( newAddress m))
    --(R (m, (SeqFL p e2):xs, v1)) -> (E (m, (SeqFR v1 ()):xs, e2))
    --(R (m, (SeqFR Void p):xs, v)) -> (R (m, xs, v))
    --(R (m, (AssigFL p e2):xs, v)) -> (E (m, (AssigFR v ()):xs, e2))
    --(R (m, (AssigFR (L n) p):xs, v)) -> (R (m2, xs, Void)) where m2 = (fromMaybe [] (update (n, v) m))
    --(R (m, (LetF id p e1):xs, v)) -> (E (m, xs, subst e1 (id, v)))
    --(R (m, (AppFL p e2):xs, v)) -> (E (m, (AppFR v ()):xs, e2))
    --(R (m, (AppFR (Fn id e3) p):xs, v)) -> (E (m, xs, subst e3 (id, v)))
    --(R (m, (IfF p e1 e2):xs, (B q))) -> if (q) then (E (m, xs, e1)) else (E (m, xs, e2))
    --(R (m, (RaiseF p):xs, v)) -> (P (m, xs, v))
    --(P (m, (HandleF p id e1):xs, (Raise e2))) -> (E (m, xs, subst e1 (id, e2)))
    --(R (m, (HandleF p id e2):xs, v)) -> (R (m, xs, v))
    --(P (m, f:xs, (Raise e1))) -> (P (m, xs, (Raise e1)))
    --(R (m, (ContinueFL p e2):xs, v)) -> (E (m, (ContinueFR v ()):xs, e2))
    --(R (m, (ContinueFR (Cont s2) p):xs, v)) -> (R (m, s2,v))
    ----evals (E ( [ ] , [ ] , Let "x" (B True) ( I f (V "x" ) (V "x" ) (B False ) ) ) )

evale :: Expr -> Expr