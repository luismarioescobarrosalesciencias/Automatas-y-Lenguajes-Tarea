module Tarea01 where
import Data.Set (Set)
import qualified Data.Set as Set


--Primera Parte: Funciones Recursivas 

esPalindromo:: Eq a => [a] -> Bool  
esPalindromo l =  False  --Implementar 

--funciones auxiliares para esPalindromo (Opcional su uso)

ultimo_elemento:: Eq a => [a] -> a
ultimo_elemento [a] = a
ultimo_elemento (_:xs) = ultimo_elemento xs

corte_:: Eq a => [a] -> [a]
corte_ [] = []
corte_ [_] = []
corte_ (_:xs) =  corte_ xs



--bolsa. Función que recibe una cadena y devuelve una lista de tuplas
--       con el número de ocurrencias de cada letra de la palabra.

--Prohibido usar Set (Eso se usara mas adelante )


bolsa:: String -> [(Char,Int)]   --Implementar 
bolsa "" = [('',-1)]  --IMPLEMENTAR 



unicos:: Eq a =>[a] -> [a] -> [a]  -- Funcion auxiliar para bolsa 
unicos [] l = l
unicos (x:xs) l2 
    | x `notElem` l2 = unicos (xs)(l2 ++ [x])
    | otherwise =  unicos xs l2


cuenta_caracter :: String -> Char -> Int  -- funcion auxiliar para bolsa 
cuenta_caracter [] _ = 0
cuenta_caracter (x:xs) a
    | x == a  = 1 + cuenta_caracter xs a
    | otherwise = cuenta_caracter xs a                  


--Segunda Parte: Cadenas y Lenguajes 

-- Definición de símbolo como un tipo nuevo
newtype Simbolo = Simbolo Char
  deriving (Eq, Ord)

-- Definición de la cadena como una lista de símbolos
newtype Cadena = Cadena [Simbolo]
  deriving (Eq)

-- Instancia personalizada de Show para Simbolo
instance Show Simbolo where
    show (Simbolo c) = [c]

-- Instancia personalizada de Show para Cadena
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

--Funciones auxiliares 

cabezaCadena :: Cadena -> Simbolo
cabezaCadena (Cadena (x:xs)) = x

quitacabeza :: Cadena -> Cadena
quitacabeza (Cadena []) = cadenaVacia
quitacabeza (Cadena (_:xs)) = Cadena xs

concatCadenaSimbolo :: Simbolo -> Cadena  -> Cadena
concatCadenaSimbolo  (Simbolo a) (Cadena w )  =  (Cadena $[Simbolo a] ++ w )   


--Ejercicios:

concatenacionC :: Cadena -> Cadena  -> Cadena  
concatenacionC (Cadena [])  w  = w
concatenacionC (Cadena u ) (Cadena w) =  w --Implementar  caso recursivo

longitudC:: Cadena -> Int  
longitudC (Cadena []) =  0 
longitudC (Cadena w) =  -1 --Implementar caso recursivo

reversaC :: Cadena -> Cadena  
reversaC (Cadena []) = (Cadena [])
reversaC (Cadena w ) = (Cadena [])    --Implementar caso recursivo


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

--EJERCICIOS


--Funcion que transforma expresiones regulares a AFNE 
toEAFN :: ER -> AFNE
toEAFN _  = "IMPLEMENTAR"


--Funcion que transforma automatas afne a automatas finitos deterministas
toAFD:: AFNE -> AFD
toAFD _ = "IMPLEMENTAR"

--Funcion que verifica si una cadena de texto es aceptada o rechazada por un AFD
readAFD:: String -> AFD -> Bool
readAFD _ _ = "IMPLEMENTAR"

 --Funcion que recibe una cadena de texto e indique si dicha cadena 
 --de texto pertenece al lenguaje generado por una expresión regular
verify:: String -> ER -> Bool
verify _ _ = "IMPLEMENTAR"



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


  

