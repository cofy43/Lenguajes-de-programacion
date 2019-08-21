data Expr = N Int 
          | T | F
          | Succ Expr | Pred Expr
          | Expr :+ Expr | Expr :− Expr
          | Expr :∗ Expr | Expr :/ Expr | Expr :% Expr
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

instance Show Expr where
          show (N n) = show n
          show T = show "True"
          show F = show "False"

instance Show Instruction where
          show (I i) = "I " ++ show i
          show (B b) = "B " ++ show b

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
                | (n == 0) = y:zs
                | length ((I n):y:z:zs) < 3 = error "No se puede CRAKC 404 not found"
                | otherwise = z:zs
stackOperation 
