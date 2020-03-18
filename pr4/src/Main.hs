import BAE.Parser as Parser
import BAE.Semantic as Semantic
import BAE.Sintax as Sintax
import BAE.Static as Static
import Data.List
import System.Environment

app1 = Sintax.Fn "x" (Sintax.App (Sintax.V "f") (Sintax.App (Sintax.V "x") (Sintax.V "x")))
fix = (Sintax.Fn "f" (Sintax.App app1 app1))

parserExprToExpr :: Parser.Expr -> Sintax.Expr
parserExprToExpr (Parser.V n) = Sintax.V n
parserExprToExpr (Parser.I x) = Sintax.I (fromIntegral x)
parserExprToExpr (Parser.B b) = Sintax.B b
parserExprToExpr (UnaryE Parser.Not e) = Sintax.Not (parserExprToExpr e)
parserExprToExpr (UnaryE Parser.Succ e) = Sintax.Succ (parserExprToExpr e)
parserExprToExpr (UnaryE Parser.Pred e) = Sintax.Pred (parserExprToExpr e)
parserExprToExpr (BinaryE Parser.And e1 e2) = Sintax.And (parserExprToExpr e1) (parserExprToExpr e2)
parserExprToExpr (BinaryE Parser.Or e1 e2) = Sintax.Or (parserExprToExpr e1) (parserExprToExpr e2)
parserExprToExpr (BinaryE Parser.Add e1 e2) = Sintax.Add (parserExprToExpr e1) (parserExprToExpr e2)
parserExprToExpr (BinaryE Parser.Mul e1 e2) = Sintax.Mul (parserExprToExpr e1) (parserExprToExpr e2)
parserExprToExpr (RelationalE Parser.Gt e1 e2) = Sintax.Gt (parserExprToExpr e1) (parserExprToExpr e2)
parserExprToExpr (RelationalE Parser.Lt e1 e2) = Sintax.Lt (parserExprToExpr e1) (parserExprToExpr e2)
parserExprToExpr (RelationalE Parser.Eq e1 e2) = Sintax.Eq (parserExprToExpr e1) (parserExprToExpr e2)
parserExprToExpr (Parser.If e1 e2 e3) = Sintax.If (parserExprToExpr e1) (parserExprToExpr e2) (parserExprToExpr e3)
parserExprToExpr (Parser.Let x e1 e2) = Sintax.Let x (parserExprToExpr e1) (parserExprToExpr e2)
parserExprToExpr (Parser.Fn ids e) = Main.curry (Parser.Fn ids e)
parserExprToExpr (Parser.RecurFn x (y:ys) e) = Sintax.App fix (Sintax.Fn x (Sintax.Fn y (parserExprToExpr e)))

curry :: Parser.Expr -> Sintax.Expr
curry (Parser.Fn [] e) = (Sintax.Fn "" (parserExprToExpr $ e))
curry (Parser.Fn (x:[]) e) = (Sintax.Fn x (parserExprToExpr e))
curry (Parser.Fn (x:xs) e) = (Sintax.Fn x (Main.curry $ Parser.Fn xs e))

parserTypeToType :: Parser.Type -> Semantic.Type
parserTypeToType (Parser.Integer) = Semantic.Integer
parserTypeToType (Parser.Boolean) = Semantic.Boolean

main = do
  args <- getArgs
  case args of
    [file] -> do
      x <- parseFile file
      let (Typed parserE parserT) = x
      let e = parserExprToExpr parserE
      let t = parserTypeToType parserT
      putStrLn "Program:"
      putStrLn $ " ⊢ " ++ (show e) ++ " : "
      putStrLn "Evaluation:"
      -- putStrLn $ show (eval e t)
      -- POR AHORA NO VERIFICA EL TIPADO.
      putStrLn $ show (evale e)
      -- -- -- -- -- -- -- -- -- -- -- --
    _ -> putStrLn "Error: Invalid file name."