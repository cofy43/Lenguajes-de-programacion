module BAE.Sintax where 

import Data.List
import Data.Char

type Identifier = String
type Subtitution = (Identifier, Expr)
type Address = Int
type Value = Expr
type Cell = (Address, Value)
type Memory = [Cell]

type Pending = ()
type Stack = [Frame]

data State = E (Memory, Stack , Expr ) 
           | R (Memory, Stack , Expr ) 
           | P (Memory, Stack , Expr ) deriving (Eq)

data Frame = SuccF Pending
           | AddFL Pending Expr
           | AddFR Expr Pending
           | MulFL Pending Expr
           | MulFR Expr Pending
           | PredF Pending
           | NotF Pending
           | RaiseF Pending
           | HandleF Pending Identifier Expr
           | AndFL Pending Expr
           | AndFR Expr Pending
           | OrFL Pending Expr
           | OrFR Expr Pending
           | LtFL Pending Expr
           | LtFR Expr Pending
           | GtFL Pending Expr
           | GtFR Expr Pending
           | EqFL Pending Expr
           | EqFR Expr Pending
           | IfF Pending Expr Expr
           | WhileF Pending Expr
           | SeqFL Pending Expr
           | SeqFR Expr Pending 
           | AllocF Pending
           | DerefF Pending
           | AssigFL Pending Expr
           | AssigFR Expr Pending
           | ContinueFL Pending Expr
           | ContinueFR Expr Pending
           | LetF Identifier Pending Expr 
           | FnF Identifier Pending
           | AppFL Pending Expr
           | AppFR Expr Pending deriving (Eq)

instance Show State where
    show e = case e of 
        (E (m, s, e)) -> "E (" ++ (show m) ++ ", " ++ (show s) ++ ", " ++ (show e) ++ ")"
        (R (m, s, e)) -> "R (" ++ (show m) ++ ", " ++ (show s) ++ ", " ++ (show e) ++ ")"
        (P (m, s, e)) -> "P (" ++ (show m) ++ ", " ++ (show s) ++ ", " ++ (show e) ++ ")"

instance Show Frame where
    show e = case e of
        (SuccF p) -> "Succ(_)"
        (AddFL p e) -> "Suma(-," ++ (show e) ++ ")"
        (AddFR e p) -> "Suma(" ++ (show e) ++ ",-)"
        (MulFL p e) -> "Mul(-," ++ (show e) ++ ")"
        (MulFR e p) -> "Mul(" ++ (show e) ++ ",-)"
        (PredF p) -> "Pred(-)"
        (NotF p) -> "Not(-)"
        (AndFL p e) -> "And(-," ++ (show e) ++ ")"
        (AndFR e p) -> "And(" ++ (show e) ++ ",-)"
        (OrFL p e) -> "Or(-," ++ (show e) ++ ")"
        (OrFR e p) -> "Or(" ++ (show e) ++ ",-)"
        (LtFL p e) -> "Lt(-," ++ (show e) ++ ")"
        (LtFR e p) -> "Lt(" ++ (show e) ++ ",-)"
        (GtFL p e) -> "Gt(-," ++ (show e) ++ ")"
        (GtFR e p) -> "Gt(" ++ (show e) ++ ",-)"
        (EqFL p e) -> "Eq(-," ++ (show e) ++ ")"
        (EqFR e p) -> "Eq(" ++ (show e) ++ ",-)"
        (IfF p e1 e2) -> "If(-," ++ (show e1) ++ "," ++ (show e2) ++ ")"
        (WhileF p e) -> "While(-," ++  (show e) ++ ")"
        (AllocF p) -> "Alloc(-)"
        (DerefF p) -> "Deref(-)"
        (AssigFL p e) -> "Assig(-," ++ (show e) ++ ")"
        (AssigFR e p) -> "Assig(" ++ (show e) ++ ",-)"
        (LetF id p e1) -> "Let(" ++ (show id) ++ ",-," ++ (show e) ++ ")"
        (FnF id p) -> "Fn(" ++ (show id) ++ ",-)"
        (AppFL p e) -> "App(-," ++ (show e) ++ ")"
        (AppFR e p) -> "App(" ++ (show e) ++ ",-)"

data Expr = Void
          | V Identifier
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
          | L Int
          | Seq Expr Expr
          | While Expr Expr
          | Alloc Expr
          | Deref Expr
          | Assig Expr Expr
          | Let Identifier Expr Expr 
          | Fn Identifier Expr
          | App Expr Expr 
          | Raise Expr
          | Handle Expr Identifier Expr 
          | LetCC Identifier Expr
          | Continue Expr Expr
          | Cont Stack deriving (Eq)

instance Show Expr where
    show e = case e of
        Void -> "Void"
        (L n) -> "L[" ++ (show n) ++ "]" 
        (Seq a b) -> "SEQ[" ++ (show a) ++ "; " ++ (show b) ++ "]"
        (While a b) -> "WHILE[" ++ (show a) ++ " " ++ (show b) ++ "]"
        (Alloc a) -> "ALLOC[" ++ (show a) ++ "]"
        (Deref a) -> "DEREF[" ++ (show a) ++ "]"
        (Assig a b) -> "ASSIG[" ++ (show a) ++ (show b) ++ "]"
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
        (Raise e) -> "Raise(" ++ (show e) ++ ")"
        (Handle e1 id e2) -> "Handle(" ++ (show e1) ++ "," ++ (show id) ++ "," ++ (show e2) ++ ")"
        (LetCC id e) -> "LetCC(" ++ (show id) ++ (show e) ++ ")"
        (Continue e1 e2) -> "Continue(" ++ (show e1) ++ (show e2) ++ ")"
        (Cont s) -> "Cont(" ++ (show s) ++ ")"

frVars :: Expr -> [Expr]
frVars e = case e of
    (Void) -> []
    (L n) -> []
    (I n) -> []
    (B b) -> []
    (V x) -> [(V x)]
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
    (Seq e1 e2) -> union (frVars e1) (frVars e2)
    (While e1 e2) -> union (frVars e1) (frVars e2)
    (Alloc e1) -> (frVars e1)
    (Deref e1) -> (frVars e1)
    (Assig e1 e2) -> union (frVars e1) (frVars e2)
    (Raise e) -> (frVars e)
    (Handle e1 id e2) -> union (frVars e1) (frVars e2)
    (LetCC id e) -> (filter (/= (V id)) (frVars e))
    (Cont s) -> head (map frVarsFrame s)

frVarsFrame :: Frame -> [Expr]
frVarsFrame e = case e of
    (SuccF p) -> []
    (AddFL p e) -> frVars e
    (AddFR e p) -> frVars e
    (MulFL p e) -> frVars e
    (MulFR e p) -> frVars e
    (PredF p) -> []
    (NotF p) -> []
    (AndFL p e) -> frVars e
    (AndFR e p) -> frVars e
    (OrFL p e) -> frVars e
    (OrFR e p) -> frVars e
    (LtFL p e) -> frVars e
    (LtFR e p) -> frVars e
    (GtFL p e) -> frVars e
    (GtFR e p) -> frVars e
    (EqFL p e) -> frVars e
    (EqFR e p) -> frVars e
    (IfF p e1 e2) -> union (frVars e1) (frVars e2)
    (WhileF p e) -> frVars e
    (AllocF p) -> []
    (DerefF p) -> []
    (AssigFL p e) -> frVars e
    (AssigFR e p) -> frVars e
    (LetF id p e1) -> (filter (/= (V id)) (frVars e1))
    (FnF id p) -> []
    (AppFL p e) -> frVars e
    (AppFR e p) -> frVars e

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
    (Void) -> Void
    (L n) -> (L n)
    (Seq a b) -> (Seq (subst a s) (subst b s))
    (While a b) -> (While (subst a s) (subst b s))
    (Alloc a) -> (Alloc (subst a s))
    (Deref a) -> (Deref (subst a s))
    (Assig a b) -> (Assig (subst a s) (subst b s))
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
    (Raise e) -> (Raise (subst e s))
    (Handle e1 id e2) -> if (y == id) then
                            (Handle (subst e1 s) id e2)
                         else if ((V id ) `elem` (frVars e1)) then
                            error "NO SE PUEDE REALIZAR LA SUBSTITUCION"
                         else 
                            (Handle (subst e1 s) id (subst e2 s))
    (LetCC id e) -> if (id == y) then
                        (LetCC id e)
                    else 
                        (LetCC id (subst e s))
    (Continue e1 e2) -> (Continue (subst e1 s) (subst e2 s))
    (Cont s) -> (Cont s)

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
alphaEq (Raise e1) (Raise e2) = (alphaEq e1 e2)
alphaEq (Handle e1 id e2) (Handle c1 ida c2) = (alphaEq e1 c1) && (alphaEq c2 (subst e2 (id, V ida)))
alphaEq (LetCC id e1) (LetCC ida e2) = (alphaEq e1 (subst e2 (id, V ida))) 
alphaEq (Continue e1 e2) (Continue c1 c2) = (alphaEq e1 c2) && (alphaEq e2 c2)
alphaEq (Cont s) (Cont b) = True
alphaEq _ _ = False

alphaExpr :: Expr -> Expr
alphaExpr (V a) = (V (incVar a))
alphaExpr (Fn id e) = (Fn (incVar id) (alphaExpr e))
alphaExpr (App e1 e2) = (App (alphaExpr e1) (alphaExpr e2))