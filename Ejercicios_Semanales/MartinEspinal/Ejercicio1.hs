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
          | Let Identifier Expr Expr deriving (Eq)

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
        (Lt a b) -> "LT[" ++ (show a) ++ " > " ++ (show b) ++ "]"
        (Gt a b) -> "Gt[" ++ (show a) ++ " < " ++ (show b) ++ "]"
        (Eq a b) -> "EQ[" ++ (show a) ++ " = " ++ (show b) ++ "]"
        (If a b c) -> "IF[" ++ (show a) ++ " then " ++ (show b) ++ " else " ++ (show c) ++ "]"
        --(Let v a b) -> "Let[" ++ (show v) ++ " " ++ (show a) ++ " " + (show b) ++ "]"
        
esVar :: Expr -> Bool
esVar (V a) = True
esVar a = False

listVar :: Expr -> Expr -> [Expr]
listVar a b = filtroA ++ filtroB
    where listA = filter (\x -> (esVar x)) [a]
          listB = filter (\x -> (esVar x)) [b]
          filtroA = [x | x <- listA,  not(x `elem` listB)]
          filtroB = [x | x <- listB,  not(x `elem` listA)] 

frVars :: Expr -> [Expr]
frVars (V v) = [V v]
frVars (I n) = []
frVars (B b) = []
frVars (Add a b) = (listVar a b)
frVars (Mul a b) = (listVar a b)
frVars (Succ a) = filter (\x -> (esVar x)) [a]
frVars (Pred a) = filter (\x -> (esVar x)) [a]
frVars (Not a) = filter (\x -> (esVar x)) [a]
frVars (And a b) = (listVar a b)
frVars (Or a b) = (listVar a b)
frVars (Lt a b) = (listVar a b)
frVars (Gt a b) = (listVar a b)
frVars (Eq a b) = (listVar a b)
--Falta resolver problema con if y agregar terminar Let
frVars (If a b c) = filtroA ++ filtroB ++ filtroC
    where listC = filter (\x -> (esVar x)) [c]
          listA = filter (\x -> (esVar x)) [a]
          listB = filter (\x -> (esVar x)) [b]
          filtroA = [x | x <- listA,  not(x `elem` listB), not(x `elem` listC)]
          filtroB = [x | x <- listB,  not(x `elem` listA), not(x `elem` listC)]
          filtroC = [x | x <- listC,  not(x `elem` listB), not(x `elem` listA)]
{- frVars (Let v a b) = [x | x <- lista, not(v `elem` lista)]
    where lista = (listVar a b) -}

subs :: Expr -> Subtitution -> Expr
subs (V v) (a, b)
    | (v == a) = b
    --Falta cachar errores 
    | otherwise = (V v)
subs (I n) (a, b) = (I n)
subs (B True) (a, b) = (B True)
subs (B False) (a, b) = (B False)
subs (Add n m) (a, b) = (Add (subs n (a, b)) (subs m (a, b)))
subs (Mul n m) (a, b) = (Mul (subs n (a,b)) (subs m (a,b)))
subs (Succ n) (a, b) = (Succ (subs n (a, b)))
subs (Pred n) (a, b) = (Pred (subs n (a, b)))
subs (Not n) (a, b) = (Not (subs n (a, b)))
subs (And n m) (a, b) = (And (subs n (a, b)) (subs m (a,b))) 
subs (Or n m) (a, b) = (Or (subs n (a, b)) (subs m (a, b)))
subs (Lt n m) (a, b) = (Lt (subs n (a, b)) (subs m (a, b)))
subs (Gt n m) (a, b) = (Gt (subs n (a, b)) (subs m (a, b)))
subs (Eq n m) (a, b) = (Eq (subs n (a, b)) (subs m (a, b)))
subs (If v n m) (a, b) = (If (subs v (a, b)) (subs n (a,b)) (subs m (a,b)))
--Falta caso Let
--subs (Let v n m) (a, b) = (Let (subs v (a, b)) (subs n (a, b)) (subs m (a, b))) 

{- alphaEq :: Expr -> Expr -> Bool
alphaEq a b 
    | ((length exprA) == (length exprB)) = True
    | otherwise = False
    where exprA = (frVars a)
          exprB = (frVars b)
 -}

--alphaEq ( Let "x" ( I 1 ) (V "x" ) ) ( Let "y" (I 1 ) (V "y" ))