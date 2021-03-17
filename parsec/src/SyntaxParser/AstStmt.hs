module SyntaxParser.AstStmt where
import Data.Map as Map ( fromList )
type Var = String

data Operator = Pow   -- Возведение в степень
              | Mult  -- Умножение
              | Div   -- Деление (с остатком)
              | Plus  -- Сложение
              | Minus -- Вычитание
              | Eq    -- Сравнение на равенство
              | Neq   -- Сравнение на неравенство
              | Le    -- Меньше или равно
              | Lt    -- Меньше
              | Ge    -- Больше или равно
              | Gt    -- Больше


instance Show Operator where
    show Pow = "^"
    show Mult = "*"
    show Div = "/"
    show Plus = "+"
    show Minus = "-"
    show Eq = "=="
    show Neq = "!="
    show Le = "<="
    show Lt = "<"
    show Ge = ">="
    show Gt = ">"

stringToOperator = Map.fromList  [
    ("^",  Pow  ),
    ("*",  Mult ),
    ("/",  Div  ),
    ("+",  Plus ),
    ("-",  Minus),
    ("==", Eq   ),
    ("!=", Neq  ),
    ("<=", Le   ),
    ("<",  Lt   ),
    (">=", Ge   ),
    (">",  Gt   )]


data Expr = Ident Var                
          | Num Int                  
          | BinOp Operator Expr Expr 


data Stmt = Ignore Expr               
          | If Expr Stmt (Maybe Stmt) 
          | While Expr Stmt           
          | Read Var                 
          | Write Expr               
          | Assign Var Expr         
          | Seq [Stmt]             


newtype Program = Program Stmt

printExpr :: Expr -> String
printExpr (Ident v) = v
printExpr (Num n) = show n
printExpr (BinOp op left right) = printExpr left ++ " " ++ show op ++ " " ++ printExpr right

printStmt :: Stmt -> String ->  String
printStmt (Ignore e) tab = tab ++ printExpr e ++ ";"
printStmt (If e st Nothing) tab = tab ++ "if " ++ "(" ++ printExpr e ++ ")" ++ "then {\n" ++ printStmt st (tab ++ "    ") ++ tab ++"\n}"
printStmt (If e st1 (Just st2)) tab = tab ++ "if " ++ "(" ++ printExpr e ++ ")" ++ " " ++ "then {\n" ++ printStmt st1 (tab ++ "    ") ++ tab ++"\n}"  ++ " else {\n" ++ printStmt st2 (tab ++ "    ") ++ tab ++ "\n}"
printStmt (While e st) tab = tab ++ "while (" ++ printExpr e ++ ") {\n" ++ printStmt st (tab ++ "    ")++ "}"
printStmt (Read v) tab = tab ++ "read(" ++ v ++ ");"
printStmt (Write e) tab = tab ++ "write(" ++ printExpr e ++ ");"
printStmt (Assign v e) tab = tab ++ v ++ " [=] " ++ printExpr e ++ ";"
printStmt (Seq sts) tab = init $ concatMap (\st -> printStmt st tab ++ "\n") sts

printer :: Program -> String
printer (Program st) = printStmt st ""

instance Show Program where
    show = printer