module BAE where

  import ParseBAE
  import Data.List

-- | BAE. Tipo que representa el conjunto de expresiones Aritmetico-Booleanas. 
  data BAE = V Name 
           | N Int 
           | B Bool
           | Suc BAE
           | Pre BAE
           | Plus BAE BAE
           | Prod BAE BAE
           | Neg BAE
           | Conj BAE BAE
           | Disy BAE BAE
           | Gt BAE BAE
           | Lt BAE BAE
           | Equi BAE BAE
           | Ift BAE BAE BAE
           | LetE Name BAE BAE

-- | Sust. Tipo que representa una sustitución de las expresiones Aritmetico-Booleanas.
  type Sust = (Name,BAE)           

-- | Ctx. Tipo que represeta al contexto para la verificacion de tipos.
type Ctx = [(Name, Type)]

-- | se instancia la clase Show para poder especificar como queremos que se impriman las
-- | expresiones Aritmetico-Booleanas. 
  instance Show BAE where
    show (V n) = n
    show (N x) = "num["++show x++"]"
    show (B b) = "bool["++show b++"]"
    show (Suc e) = "suc("++show e++")"
    show (Pre e) = "pred("++show e++")"
    show (Plus e1 e2) = "plus("++show e1++","++show e2++")"
    show (Prod e1 e2) = "prod("++show e1++","++show e2++")"
    show (Neg e) = "neg("++show e++")"
    show (Conj e1 e2) = "and("++show e1++","++show e2++")"
    show (Disy e1 e2) = "or("++show e1++","++show e2++")"
    show (Gt e1 e2) = "gt("++show e1++","++show e2++")"
    show (Lt e1 e2) = "lt("++show e1++","++show e2++")"
    show (Equi e1 e2) = "eq("++show e1++","++show e2++")"
    show (Ift e1 e2 e3) = "if("++show e1++","++show e2++","++show e3++")"
    show (LetE x e1 e2) = "let("++show e1++","++x++"."++show e2++")"

-- | transform. funcion que dada una declaracion devuelve su expresion Aritmetico-Booleana correspondiente.
  transform_bae :: Stmt -> BAE
  transform_bae (Var n) = V n
  transform_bae (Num x) = N (fromIntegral x)
  transform_bae (BoolCt b) = B b
  transform_bae (ABUnary Not e) = Neg (transform_bae e)
  transform_bae (ABUnary Succ e) = Suc (transform_bae e)
  transform_bae (ABUnary Pred e) = Pre (transform_bae e)
  transform_bae (ABBinary And e1 e2) = Conj (transform_bae e1) (transform_bae e2)
  transform_bae (ABBinary Or e1 e2) = Disy (transform_bae e1) (transform_bae e2)
  transform_bae (ABBinary Add e1 e2) = Plus (transform_bae e1) (transform_bae e2)
  transform_bae (ABBinary Times e1 e2) = Prod (transform_bae e1) (transform_bae e2)
  transform_bae (ABRel GrT e1 e2) = Gt (transform_bae e1) (transform_bae e2)
  transform_bae (ABRel LowT e1 e2) = Lt (transform_bae e1) (transform_bae e2)
  transform_bae (ABRel Equ e1 e2) = Equi (transform_bae e1) (transform_bae e2)
  transform_bae (If e1 e2 e3) = Ift (transform_bae e1) (transform_bae e2) (transform_bae e3)
  transform_bae (Let x e1 e2) = LetE x (transform_bae e1) (transform_bae e2)

  transform :: StmtT -> (BAE,Type)
  transform (Typed s t) = (transform_bae s,t) 

-- | fv. funcion que devuelve el conjunto de variables libres de una expresion Aritmetico-Booleanas.
  fv :: BAE -> [Name]
  fv e = error "error"

-- | substitution. funcion que devuelve la expresion Aritmetico-Booleanas resultante de aplicar
-- | una sustitucion a la misma. 
  substitution :: BAE -> Sust -> BAE
  substitution e s = error "error"

-- | vt. funcion que verifica que el tipo de una expresion sea valido dado un contexto.
  vt :: Ctx -> BAE -> Type -> Bool
  vt c e t = error "error" 

-- | eval1. funcion que devuelve un paso en la evalucion de una expresion 
-- | Aritmetico-Booleana a evaluar.
  eval1 :: BAE -> BAE
  eval1 e = error $ "Execution Error: Locked state."

-- | evals. funcion que devuelve 0 o mas pasos de la evaluacion de una expresion
-- | Aritmetico-Booleana.
  evals :: BAE -> BAE
  evals e = error "error"

-- | isValue. funcion que determina si una expresion Aritmetico-Booleana es valor.
  isValue :: BAE -> Bool
  isValue e = error "error"

-- | eval. funcion que devuelve el valor de la evaluacion de una formula Aritmetico-Booleana.
  eval :: BAE -> Type -> BAE
  eval e = error "error"
