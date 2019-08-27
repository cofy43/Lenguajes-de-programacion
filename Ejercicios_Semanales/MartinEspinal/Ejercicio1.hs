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
frVars (If a b c) = filtroA ++ filtroB ++ filtroC
    where listC = filter (\x -> (esVar x)) [c]
          listA = filter (\x -> (esVar x)) [a]
          listB = filter (\x -> (esVar x)) [b]
          filtroA = [x | x <- listA,  not(x `elem` listB), not(x `elem` listC)]
          filtroB = [x | x <- listB,  not(x `elem` listA), not(x `elem` listC)]
          filtroC = [x | x <- listC,  not(x `elem` listB), not(x `elem` listA)]
frVars (Let v a b) = [x | x <- lista, not((V v) `elem` lista)]
    where lista = (listVar a b)

subst :: Expr -> Subtitution -> Expr
subst (V v) (a, b)
    | (v == a) = b
subst (I n) (a, b) = (I n)
subst (B True) (a, b) = (B True)
subst (B False) (a, b) = (B False)
subst (Add n m) (a, b) = (Add (subst n (a, b)) (subst m (a, b)))
subst (Mul n m) (a, b) = (Mul (subst n (a,b)) (subst m (a,b)))
subst (Succ n) (a, b) = (Succ (subst n (a, b)))
subst (Pred n) (a, b) = (Pred (subst n (a, b)))
subst (Not n) (a, b) = (Not (subst n (a, b)))
subst (And n m) (a, b) = (And (subst n (a, b)) (subst m (a,b))) 
subst (Or n m) (a, b) = (Or (subst n (a, b)) (subst m (a, b)))
subst (Lt n m) (a, b) = (Lt (subst n (a, b)) (subst m (a, b)))
subst (Gt n m) (a, b) = (Gt (subst n (a, b)) (subst m (a, b)))
subst (Eq n m) (a, b) = (Eq (subst n (a, b)) (subst m (a, b)))
subst (If v n m) (a, b) = (If (subst v (a, b)) (subst n (a,b)) (subst m (a,b)))
subst (Let v n m) (a, b) = (Let v (subst n (a, b)) (subst m (a, b))) 

alphaEq :: Expr -> Expr -> Bool
alphaEq (V v) (V b)
    | (v == b) = True
    | otherwise = False
alphaEq (I n) (I m)
    | (n == m) = True
    | otherwise = False
alphaEq (B True) (B True) = True
alphaEq (B False) (B False) = True
alphaEq (B True) (B False) = False
alphaEq (B False) (B True) = False
alphaEq (Add a b) (Add c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (Mul a b) (Mul c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (Succ a) (Succ b) = (alphaEq a b)
alphaEq (Pred a) (Pred b) = (alphaEq a b)
alphaEq (Not a) (Not b) = (alphaEq a b)
alphaEq (Add a b) (Add c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (Or a b) (Or c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (Lt a b) (Lt c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (Gt a b) (Gt c d) = (alphaEq a c) && (alphaEq b d)
alphaEq (Let v a b) (Let g c d)
    | ((length listA) /= (length listB)) = False
    | otherwise = (alphaEq a c) && (alphaEq b (subst d (v, a)))
    where listA = ((frVars a) ++ (frVars b))
          listB = ((frVars c) ++ (frVars d))