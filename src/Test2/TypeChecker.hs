module TypeChecker where

-- =========================================================
-- 1. TIPOS Y AST DEL EQUIPO (Copiado de PrettyPrinter)
-- =========================================================
type Identifier = String

data LType = R | B | Fun LType LType deriving (Eq)

instance Show LType where
  show R           = "R"
  show B           = "B"
  show (Fun t1 t2) = "(" ++ show t1 ++ " -> " ++ show t2 ++ ")"

data Expr
  = Var Identifier
  | LitDouble Double
  | LitBool Bool
  -- Secuenciación y Asignaciones
  | Seq Expr Expr
  | Let Identifier LType Expr
  | Assign Identifier Expr
  -- Condicionales y Abstracciones
  | If Expr Expr Expr
  | Abs Identifier LType Expr
  -- Operaciones Aritméticas
  | Add Expr Expr
  | Sub Expr Expr
  | Mul Expr Expr
  | Div Expr Expr
  | Pow Expr Expr
  | Neg Expr
  -- Aplicación y Print
  | App Expr Expr
  | Print Expr
  deriving (Show, Eq)

-- =========================================================
-- 2. EL ENTORNO (Para guardar las variables)
-- =========================================================
type Env = [(Identifier, LType)]

lookupType :: Identifier -> Env -> Either String LType
lookupType varName env = case lookup varName env of
  Just t  -> Right t
  Nothing -> Left ("Error: Variable no definida - '" ++ varName ++ "'")

-- =========================================================
-- 3. EL TYPE CHECKER (Análisis Estático Completo)
-- =========================================================
typeCheck :: Env -> Expr -> Either String LType
typeCheck env expr = case expr of

  -- ==================== BÁSICOS ====================
  LitDouble _ -> Right R
  LitBool _   -> Right B
  Var x       -> lookupType x env
  Print e     -> typeCheck env e

  -- ==================== ARITMÉTICA ====================
  Add e1 e2 -> checkMath env e1 e2 "+"
  Sub e1 e2 -> checkMath env e1 e2 "-"
  Mul e1 e2 -> checkMath env e1 e2 "*"
  Div e1 e2 -> checkMath env e1 e2 "/"
  Pow e1 e2 -> checkMath env e1 e2 "^"
  
  Neg e -> do
    t <- typeCheck env e
    if t == R then Right R else Left "Error de Tipo: Negacion (-) requiere un R."

  -- ==================== CONDICIONAL ====================
  If cond eThen eElse -> do
    tCond <- typeCheck env cond
    if tCond == R || tCond == B
      then do
        tThen <- typeCheck env eThen
        tElse <- typeCheck env eElse
        if tThen == tElse
          then Right tThen
          else Left ("Error de Tipo: Ramas del if tienen tipos distintos. Then: " ++ show tThen ++ ", Else: " ++ show tElse)
      else Left "Error de Tipo: La condicion del If debe ser un R o B."

  -- ==================== CÁLCULO LAMBDA ====================
  -- Abstracción: Creamos un nuevo ámbito agregando (x, t) al entorno actual
  Abs x t body -> do
    let nuevoEntorno = (x, t) : env
    tBody <- typeCheck nuevoEntorno body
    Right (Fun t tBody)

  -- Aplicación: f debe ser una función, y el tipo del argumento debe coincidir
  App f arg -> do
    tF <- typeCheck env f
    tArg <- typeCheck env arg
    case tF of
      Fun tParam tRet -> 
        if tParam == tArg
          then Right tRet
          else Left ("Error de Tipo: La funcion espera un argumento de tipo " ++ show tParam ++ " pero recibio " ++ show tArg)
      _ -> Left "Error de Tipo: Intentaste aplicar un argumento a algo que no es una funcion."

  -- ==================== SECUENCIAS Y VARIABLES ====================
  -- Si el primer elemento de la secuencia es un Let, inyectamos la variable en el resto de la secuencia
  Seq (Let x t e1) e2 -> do
    tE1 <- typeCheck env e1
    if tE1 == t
      then typeCheck ((x, t) : env) e2
      else Left ("Error de Tipo: Declaraste " ++ x ++ " como " ++ show t ++ " pero su valor es " ++ show tE1)

  -- Si es una secuencia normal, simplemente verificamos e1 por errores y devolvemos el tipo de e2
  Seq e1 e2 -> do
    _ <- typeCheck env e1 
    typeCheck env e2

  -- Si el Let está suelto (no en una secuencia), simplemente devolvemos su tipo
  Let x t e -> do
    tE <- typeCheck env e
    if tE == t then Right t else Left ("Error de Tipo en Let de " ++ x)

  -- Asignación a variables mutables (verificamos que el nuevo valor coincida con el tipo original)
  Assign x e -> do
    tOriginal <- lookupType x env
    tNuevo <- typeCheck env e
    if tOriginal == tNuevo
      then Right tNuevo
      else Left ("Error de Tipo: La variable " ++ x ++ " es de tipo " ++ show tOriginal ++ " pero intentas asignarle " ++ show tNuevo)


-- =========================================================
-- Función auxiliar para aritmética
-- =========================================================
checkMath :: Env -> Expr -> Expr -> String -> Either String LType
checkMath env e1 e2 opName = do
  t1 <- typeCheck env e1
  t2 <- typeCheck env e2
  if t1 == R && t2 == R
    then Right R
    else Left ("Error de Tipo: '" ++ opName ++ "' requiere dos numeros (R).")