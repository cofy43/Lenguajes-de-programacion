-- Declaracion de los tipos para el interprete
data Instruction = N Int 
                 | Verdadero
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

-- Defición de los tipos para el interprete
type Program = [Instruction]
type Stack = [Instruction]
