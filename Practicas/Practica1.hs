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
          | ADD | AND | DIV | Eq | EXEC | GET | Gt
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

arithOperation :: Instruction -> Instruction -> Instruction -> Instruction
arithOperation (I x) (I y) ADD = (I (x + y))
arithOperation (I x) (I y) MUL = (I (x * y))
arithOperation (I x) (I y) DIV 
        | (y == 0) = error "División por cero"
        | otherwise = (I (div x y))
arithOperation (I x) (I y) REM 
        | (y == 0) = error "Division por cero" 
        | otherwise = (I (rem x y))
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
        | length(x:y:xs) >= 2 = (y:x:xs)
        | otherwise = error "Estmado usuario, no hay suficientes elementos en el Stack"
stackOperation xs (ES (ys)) = [ES (ys)] ++ xs
stackOperation (x:xs) POP = (xs)
stackOperation [] POP = error "Estimado usuario, el Stack se encuentra vacio"
stackOperation ((I n):xs) GET
        | (n > 1 && n < length xs) = (xs !! (n - 1)):xs
        | otherwise = error "Estimado usuario, no hay suficientes elementos en el Stack"
stackOperation ((I n):y:z:zs) SEL
                | (n == 0) = y:zs
                | length ((I n):y:z:zs) < 3 = error "No se puede CRAKC 404 not found"
                | otherwise = z:zs

execOperation :: [Instruction] -> Stack -> Instruction -> ([Instruction], Stack)
execOperation xs ((ES (ys)):zs) EXEC = ((ys ++ xs), zs)
execOperation xs (y:ys) EXEC = error "Sólo se pueden ejecutar secuencias de comandos"
execOperation xs [] EXEC = error "La pila está vacía"

executeProgram :: Program -> Stack -> Stack
executeProgram [] s = s
executeProgram ((I n):xs) s = executeProgram xs ((I n):s)
executeProgram ((B b):xs) s = executeProgram xs ((B b):s)
executeProgram (ADD:xs) ((I n):(I m):ys) = executeProgram xs ((arithOperation (I n) (I m) ADD):ys)
executeProgram (MUL:xs) ((I n):(I m):ys) = executeProgram xs ((arithOperation (I n) (I m) MUL):ys)
executeProgram (DIV:xs) ((I n):(I m):ys) = executeProgram xs ((arithOperation (I n) (I m) DIV):ys)
executeProgram (SUB:xs) ((I n):(I m):ys) = executeProgram xs ((arithOperation (I n) (I m) SUB):ys)
executeProgram (REM:xs) ((I n):(I m):ys) = executeProgram xs ((arithOperation (I n) (I m) REM):ys)

executeProgram (AND:xs) ((B p):(B q):ys) = executeProgram xs ((bboolOperation (B p) (B q) AND):ys)
executeProgram (NOT:xs) ((B p):ys) = executeProgram xs ((uboolOperation (B p) NOT):ys)

executeProgram (Gt:xs) ((I n):(I m):ys) = executeProgram xs ((relOperation (I n) (I m) Gt):ys)
executeProgram (Lt:xs) ((I n):(I m):ys) = executeProgram xs ((relOperation (I n) (I m) Lt):ys)
executeProgram (Eq:xs) ((I n):(I m):ys) = executeProgram xs ((relOperation (I n) (I m) Eq):ys)

executeProgram (SWAP:xs) s = executeProgram xs (stackOperation s SWAP)
executeProgram ((ES xs):ys) s = executeProgram ys (stackOperation s (ES xs))
executeProgram (POP:xs) s = executeProgram xs (stackOperation s POP)
executeProgram (GET:xs) s = executeProgram xs (stackOperation s GET)
executeProgram (SEL:xs) s = executeProgram xs (stackOperation s SEL)

executeProgram (EXEC:xs) s = executeProgram (fst x) (snd x) where x = (execOperation xs s EXEC)