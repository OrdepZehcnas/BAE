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

-- | Tipo. Tipo que representa a los tipos de nuestro lenguaje.
data Tipo = NAT | BOOL

-- | Ctx. Tipo que represeta al contexto para la verificacion de tipos.
type Ctx = [(Name, Tipo)]

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
  transform :: Stmt -> BAE
  transform (Var n) = V n
  transform (Num x) = N (fromIntegral x)
  transform (BoolCt b) = B b
  transform (ABUnary Not e) = Neg (transform e)
  transform (ABUnary Succ e) = Suc (transform e)
  transform (ABUnary Pred e) = Pre (transform e)
  transform (ABBinary And e1 e2) = Conj (transform e1) (transform e2)
  transform (ABBinary Or e1 e2) = Disy (transform e1) (transform e2)
  transform (ABBinary Add e1 e2) = Plus (transform e1) (transform e2)
  transform (ABBinary Times e1 e2) = Prod (transform e1) (transform e2)
  transform (ABRel GrT e1 e2) = Gt (transform e1) (transform e2)
  transform (ABRel LowT e1 e2) = Lt (transform e1) (transform e2)
  transform (ABRel Equ e1 e2) = Equi (transform e1) (transform e2)
  transform (If e1 e2 e3) = Ift (transform e1) (transform e2) (transform e3)
  transform (Let x e1 e2) = LetE x (transform e1) (transform e2)    

-- | fv. funcion que devuelve el conjunto de variables libres de una expresion Aritmetico-Booleanas.
  fv :: BAE -> [Name]
  fv e = error "error"

-- | substitution. funcion que devuelve la expresion Aritmetico-Booleanas resultante de aplicar
-- | una sustitucion a la misma. 
  substitution :: BAE -> Sust -> BAE
  substitution e s = error "error"

-- | eval1. funcion que devuelve un paso en la evalucion de una expresion 
-- | Aritmetico-Booleana a evaluar.
  eval1 :: BAE -> BAE
  eval1 e = error $ "Execution Error: Locked state."

-- | evals. funcion que devuelve 0 o mas pasos de la evaluacion de una expresion
-- | Aritmetico-Booleana.
  evals :: BAE -> BAE
  evals e = error "error"

-- | vt. funcion que dato una expresion y un tipo verifica si esta bien formada.
  vt :: Ctx -> BAE -> Tipo -> Bool
  vt _ _ _ = False

-- | isValue. funcion que determina si una expresion Aritmetico-Booleana es valor.
  isValue :: BAE -> Bool
  isValue e = error "error"

-- | eval. funcion que devuelve el valor de la evaluacion de una formula Aritmetico-Booleana.
  eval :: BAE -> BAE
  eval e = error "error"
