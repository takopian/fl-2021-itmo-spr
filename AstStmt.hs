module AstStmt where

-- Имена переменных
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

-- Выражения (expressions)
data Expr = Ident Var                -- Идентификатор
          | Num Int                  -- Число
          | BinOp Operator Expr Expr -- Выражение с бинарным оператором

-- Инструкции (statements)
data Stmt = Ignore Expr               -- Инструкция, которая является выражением (подразумевается, что значение выражения игнорируется)
          | If Expr Stmt (Maybe Stmt) -- Условное выражение. Первый операнд -- условие, второй -- ветка true, третий -- опциональная ветка else
          | While Expr Stmt           -- Цикл с предусловием. Первый операнд -- условие
          | Read Var                  -- Прочитать значение переменной
          | Write Expr                -- Напечатать значение выражения
          | Assign Var Expr           -- Присвоить значение выражения переменной
          | Seq [Stmt]                -- Последовательность инструкций

-- Абстрактное синтаксическое дерево программы
newtype Program = Program Stmt -- Программа является инструкцией

printExpr :: Expr -> String
printExpr (Ident v) = v
printExpr (Num n) = show n
printExpr (BinOp op left right) = "(" ++ printExpr left ++ show op ++ printExpr right ++ ")"

printStmt :: Stmt -> String
printStmt (Ignore e) = printExpr e ++ ";"
printStmt (If e st Nothing) = "if " ++ "(" ++ printExpr e ++ ")" ++ "then {\n" ++ printStmt st ++ "}"
printStmt (If e st1 (Just st2)) = "if " ++ "(" ++ printExpr e ++ ")" ++ "then {\n" ++ printStmt st1 ++ "}"  ++ " else {\n" ++ printStmt st2 ++ "}"
printStmt (While e st) = "while (" ++ printExpr e ++ ") {\n" ++ printStmt st ++ "}"
printStmt (Read v) = "read(" ++ v ++ ");"
printStmt (Write e) = "write(" ++ printExpr e ++ ");"
printStmt (Assign v e) = v ++ "[=]" ++ printExpr e ++ ";"
printStmt (Seq sts) = concatMap (\st -> printStmt st ++ "\n") sts

printer :: Program -> String
printer (Program st) = printStmt st