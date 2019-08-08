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
                 deriving (Eq,Ord)
-- Defición de los tipos para el interprete
type Program = [Instruction]
type Stack = [Instruction]

instance Show Instruction where
 show Verdadero = "V"
 show Falso = "F"
 show (N n) = show n

addOperation :: Instruction -> Instruction -> Instruction
addOperation (N n) (N m) = (N (n + m))
addOperation (N n) x = error "Estimado usuario no es posible sumar un numero con una operacion"
addOperation x (N n) = error "Estimado usuario no es posible sumar un numero con una operacion"
addOperation x y = error "Estimado usuario no es posible sumar dos operaciones"
