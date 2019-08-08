-- Declaracion de los tipos para el interprete

data Instruction = N Int 
                 | Verdadero
                 | Falso
                 | ADD
                 | DIV 
                 | EQ  
                 | EXEC
                 | GT
                 | LT
                 | MUL
                 | NGET
                 | POP
                 | REM
                 | SER
                 | SOB
                 | SOLAP
                 | E [Instruction]
                 deriving (Eq,Ord)
-- Defición de los tipos para el interprete
type Program = [Instruction]
type Stack = [Instruction]

instance Show Instruction where
 show Verdadero = "True"
 show Falso = "False"
 show (N n) = show n

addOperation :: Instruction -> Instruction -> Instruction
addOperation (N n) (N m) = (N (n + m))
addOperation (N n) x = error "Estimado usuario no es posible sumar un numero con una operacion"
addOperation x (N n) = error "Estimado usuario no es posible sumar un numero con una operacion"
addOperation x y = error "Estimado usuario no es posible sumar dos operaciones"

divOperation :: Instruction -> Instruction -> Instruction
divOperation (N n) (N m) = (N (div n m))
divOperation (N n) x = error "Estimado usuario no es posible dividir un numero con una operacion"
divOperation x (N n) = error "Estimado usuario no es posible dividir un numero con una operacion"
divOperation x y = error "Estimado usuario no es posible dividir dos operaciones"

mulOperation :: Instruction -> Instruction -> Instruction
mulOperation (N n) (N m) = (N (n * m))
mulOperation (N n) x = error "Estimado usuario no es posible multiplicar un numero con una operacion"
mulOperation x (N n) = error "Estimado usuario no es posible multiplicar un numero con una operacion"
mulOperation x y = error "Estimado usuario no es posible multiplicar dos operaciones"

gtOperation :: Instruction -> Instruction -> Instruction
gtOperation (N n) (N m)
        | n < m     = (N m)
        | otherwise = (N n)
gtOperation (N n) x = error "Estimado usuario no es posible comparar un numero con una operacion"
gtOperation x (N n) = error "Estimado usuario no es posible comparar un numero con una operacion"
gtOperation x y = error "Estimado usuario no es posible comparar operaciones"

ltOperation :: Instruction -> Instruction -> Instruction
ltOperation (N n) (N m)
        | n > m     = (N m)
        | otherwise = (N n)
ltOperation (N n) x = error "Estimado usuario no es posible comparar un numero con una operacion"
ltOperation x (N n) = error "Estimado usuario no es posible comparar un numero con una operacion"
ltOperation x y = error "Estimado usuario no es posible comparar operaciones"

popOperation :: Stack -> Instruction
popOperation []  = error "El stack se encuentra vacia"
popOperation [x] = x
popOperation (x:xs) = x
 

{- selOperation :: Stack -> Stack
selOperation [] = error "El stack se encuentra vacia"
selOperation (x:y:ys)
        | (x == 0)  = [y]
        | otherwise = (ys) -}