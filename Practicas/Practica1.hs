data Expr = N Int 
          | T | F
          | Succ Expr | Pred Expr
          | Expr :+ Expr | Expr :- Expr
          | Expr :* Expr | Expr :/ Expr | Expr :% Expr
          | Not Expr | Expr :& Expr | Expr :| Expr
          | Expr :> Expr | Expr :< Expr | Expr := Expr
          | Expr :^ Expr
          | Max Expr Expr | Min Expr Expr
          | Fact Expr

data Instruction = I Int | B Bool
          | ADD | AND | DIV | Eq | EXEC | GET| Gt
          | Lt | MUL | NOT | POP | REM | SEL | SUB 
          | SWAP | ES [Instruction]

type Stack = [Instruction]

type Program = [Instruction]

instance Show Expr where
          show (N n) = show n
          show T = show "True"
          show F = show "False"

instance Show Instruction where
          show (I i) = "I " ++ show i
          show (B b) = "B " ++ show b
          show (ADD) = "ADD"
          show (AND) = "AND"
          show (DIV) = "DIV"
          show (Eq) = "Eq"
          show (EXEC) = "EXEC"
          show (GET) = "GET"
          show (Gt) = "Gt"
          show (Lt) = "Lt"
          show (MUL) = "MUL"
          show (NOT) = "NOT"
          show (POP) = "POP"
          show (REM) = "REM"
          show (SEL) = "SEL"
          show (SUB) = "SUB"
          show (SWAP) = "SWAP"
          show (ES (xs)) = "ES" ++ show xs

arithOperation :: Instruction -> Instruction -> Instruction -> Instruction
arithOperation (I x) (I y) ADD = (I (x + y))
arithOperation (I x) (I y) DIV = (I (div x y))
arithOperation (I x) (I y) MUL = (I (x * y))
arithOperation (I x) (I y) REM = (I (rem x y))
arithOperation (I x) (I y) SUB = (I (x - y))
arithOperation x y z = error "Estimado usuario los parametros no son validos"

bboolOperation :: Instruction -> Instruction -> Instruction -> Instruction
bboolOperation (B b) (B a) AND = (B (b && a))
bboolOperation b a x = error "Estimado usuario los parametros no son validos"

uboolOperation :: Instruction -> Instruction -> Instruction
uboolOperation (B b) NOT = (B (not b))
uboolOperation b a = error "Estimado usuario los parametros no son validos"

relOperation :: Instruction -> Instruction -> Instruction -> Instruction
relOperation (I a) (I b) Eq = (B (a == b))
relOperation (I a) (I b) Gt = (B (a > b))
relOperation (I a) (I b) Lt = (B (a < b))
relOperation a b c = error "Estimado usuario los parametros no son validos"

stackOperation :: Stack -> Instruction -> Stack
stackOperation (x:y:xs) SWAP 
        |length(x:y:xs) > 2 = (y:x:xs)
        | otherwise = error "Estmado usuario, no hay suficientes elementos en el Stack"
stackOperation (x:xs) (ES (y:ys)) = [ES (y:ys)] ++ (x:xs)
stackOperation (x:xs) POP = (xs)
stackOperation [] POP = error "Estimado usuario, el Stack se encuentra vacio"
stackOperation ((I n):xs) GET
        | (n > 1 && n < length xs) = (xs !! (n - 1)):xs
        | otherwise = error "Estimado usuario, no hay suficientes elementos en el Stack"
stackOperation ((I n):y:z:zs) SEL
                | (n > 0) = y:zs
                | length ((I n):y:z:zs) < 3 = error "No se puede CRAKC 404 not found"
                | otherwise = z:zs
                
compile :: Expr -> Program
compile (N a) = [(I a)]
compile T = [(B True)]
compile F = [(B False)]
compile (Succ (N a)) = [(I a), (I 1), ADD]
compile (Pred (N a)) = [(I a), (I 1), SUB]
compile (a :+ b) = (compile a) ++ (compile b) ++ [ADD]
compile (a :- b) = (compile a) ++ (compile b) ++ [SUB]
compile (a :* b) = (compile a) ++ (compile b) ++ [MUL]
compile (a :/ b) = (compile a) ++ (compile b) ++ [DIV]
compile (a :% b) = (compile a) ++ (compile b) ++ [REM]
compile (Not a) = (compile a) ++ [NOT]
compile (a :& b) = (compile a) ++ (compile b) ++ [AND]
compile (a :| b) = [NOT] ++ (compile a) ++ [NOT] ++ (compile b) ++ [AND] ++ [NOT]
compile (a :> b) = (compile a) ++ (compile b) ++ [Gt]
compile (a :< b) = (compile a) ++ (compile b) ++ [Lt]
compile (a := b) = (compile a) ++ (compile b) ++ [Eq]
compile (a :^ b) = (compile a) ++ (compile b) ++ [Eq] ++ [NOT]
compile (Max a b) = (compile a) ++ (compile b) ++ (compile a) ++ (compile b) ++ [Gt] ++ [SEL]
compile (Min a b) = (compile a) ++ (compile b) ++ (compile a) ++ (compile b) ++ [Lt] ++ [SEL]
compile (Fact (N a)) = [ES (factAux (N a))]


factAux :: Expr -> Program
factAux (N 0) = [(I 1)]
factAux (N 1) = [(I 1)]
factAux (N n) = (factAux (N (n-1))) ++ [(I n)] ++ [MUL]