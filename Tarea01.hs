module Tarea01 where
import Data.Set (Set)
import qualified Data.Set as Set

-- Definición de símbolo como un tipo nuevo
data Simbolo = ASCII Char | EpsilonSy
  deriving (Eq, Ord)

-- Definición de alfabeto como un conjunto de símbolos
newtype Alfabeto = Alfabeto [Simbolo]
  deriving (Eq, Show)

-- Definición de palabra como una lista de símbolos
newtype Cadena = Cadena [Simbolo]
  deriving (Eq)

-- Definición de lenguaje como una lista de palabras
newtype Lenguaje = Lenguaje [Cadena]
  deriving (Eq, Show)

-- Instancia personalizada de Show para Simbolo
instance Show Simbolo where
    show (ASCII c) = [c]
    show EpsilonSy = "ε"

-- Instancia personalizada de Show para Palabra
instance Show Cadena where
    show (Cadena []) = "ε"
    show (Cadena simbolos) = concatMap show simbolos

-- Definición de la cadena vacía
cadenaVacia :: Cadena
cadenaVacia = Cadena []

-- Función para verificar si una cadena es vacía
esVacia :: Cadena -> Bool
esVacia (Cadena []) = True
esVacia _ = False

-- Función para obtener el símbolo más a la derecha de una cadena
first :: Cadena -> Simbolo
first (Cadena []) = EpsilonSy
first (Cadena simbolos) = "Implementar"

-- Función para obtener el prefijo de una cadena (sin el último símbolo)
prefix :: Cadena -> Cadena
prefix (Cadena []) = cadenaVacia
prefix (Cadena simbolos) = "Implementar"

-- Función para verificar si una palabra pertenece a un lenguaje
membership :: Cadena -> Lenguaje -> Bool
membership p (Lenguaje palabras) = "Implementar"

--Expresiones Regulares y Automatas 
--A partir de aqui la cadenas y simbolos de alfabetos seran representados por ASCII

-- Definición de los estados
data State = Q Int | Qs [Int] 
    deriving (Show, Eq,Ord)

-- Definición de la estructura de datos para representar expresiones regulares
data ER = Empty             -- Representa la er vacía    
        | Epsilon           -- Representa el lenguaje {ε}
        | Sym Char          -- Representa un símbolo ASCII 
        | Conc ER ER        -- Representa la concatenación de dos er
        | Union ER ER       -- Representa la unión   de dos er
        | Kleene ER         -- Representa la estrella  de Kleene  de una er
    deriving (Eq)

instance Show ER where
    show Empty       = "∅"                   -- Vacio 
    show Epsilon     = "ε"                   -- Representando epsilon
    show (Sym c)     = [c]                   -- Un solo carácter
    show (Conc e1 e2) = "(" ++ show e1 ++ " . " ++ show e2 ++ ")" -- Concatenación
    show (Union e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")" -- Union
    show (Kleene e)   = "(" ++ show e ++ ")*" -- Estrella de Kleene

-- Definición de AFD (Autómata Finito Determinista)
data AFD = AFD {
  afdStates   :: State,                  -- Conjunto de estados
  afdAlphabet :: String,                   -- Alfabeto (conjunto de caracteres)
  afdDelta    :: State -> Char -> State,   -- Función de transición
  afdInit     :: State,                    -- Estado inicial
  afdFinal    :: State                  -- Conjunto de estados finales
}

-- Definición de AFNE (Autómata Finito No Determinista con transiciones epsilon)
data AFNE = AFNE {
  afneStates   :: State,                             -- Conjunto de estados
  afneAlphabet :: String,                              -- Alfabeto (conjunto de caracteres)
  afneDelta    :: State -> CharWithEpsilon -> [State], -- Función de transición con epsilon
  afneInit     :: State,                               -- Estado inicial
  afneFinal    :: State                              -- Conjunto de estados finales
}

-- Definición de CharWithEpsilon para AFNE
data CharWithEpsilon = Car Char | EpsilonTransition 
    deriving (Show, Eq)

-- Función para generar un nuevo estado
nuevoEstado :: Int -> State
nuevoEstado n = Q n


--Funcion que dada una lista regresa el conjunto potencia de sus elementos (Sobre listas)
conjuntoPotencia :: [a] -> [[a]]
conjuntoPotencia []     = [[]]
conjuntoPotencia (x:xs) = conjuntoPotencia xs ++ [x:ps | ps <- conjuntoPotencia xs]

--Funcion que calcula la la clausura epsilon de un estado
closure:: State -> AFNE -> [State]
closure q afne = Set.toList( closure_aux (Set.singleton q) afne )

closure_aux:: Set State -> AFNE -> Set State
closure_aux cls afne = 
    let
    epsilonTransitions = Set.fromList [q' | q <- Set.toList cls,
                                            q' <- afneDelta afne q EpsilonTransition]
    
    -- Actualizamos nuestro conjunto de estados 
    newStates = Set.union cls epsilonTransitions
  in
    -- Si no hay nada nuevo entonces regresar cls , si no continuar con la recursion
    if newStates == cls
    then cls
    else closure_aux newStates afne 


--IMPLEMENTA LAS FUNCIONES QUE SE SOLICITAN EN EL PDF


--Ejemplo de como funciona la cerradura en un AFNE:

main :: IO ()
main = do
    -- Paso 1: Definimos los estados
    let q0 = Q 0
        q1 = Q 1
        q2 = Q 2
        q3 = Q 3
        q4 = Q 4

    -- Paso 2: Definimos las deltas 
    let delta :: State -> CharWithEpsilon -> [State]
        delta (Q 0) EpsilonTransition = [q1, q2] 
        delta (Q 1) (Car 'a') = [q1]            
        delta (Q 2) (Car 'b') = [q2,q4]            
        delta (Q 1) EpsilonTransition = [q3]    
        delta (Q 4) (Car 'a' ) = [q0]
        delta _ _ = []                           

    -- Paso 3: Construimos el afne 
    let afne = AFNE {
            afneStates = Qs [0, 1, 2, 3],
            afneAlphabet = "ab",
            afneDelta = delta,
            afneInit = q0,
            afneFinal = q3
        }

    -- Paso 4: Aplicamos la cerradura a q0
    let closureResult = closure q0 afne

    -- Paso 5: Imprimimos el resultado
    putStrLn $ "CLS epsilon de q0: " ++ show closureResult


  

