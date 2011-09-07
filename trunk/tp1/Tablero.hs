module Tablero where

import Char
import Maybe

data Color = Blanco | Negro deriving (Show, Eq, Ord)
type Posicion = (Char, Int) 
data Tablero = T (Posicion -> Maybe Color)
instance Eq Tablero where
   T f1 == T f2 = all(\x -> contenido x(T f1) == contenido x(T f2) ) posiciones


instance Show Tablero where
  show (T tablero) = 
      "   a b c d e f g h  \n" ++
      "  ----------------- \n" ++
      concatMap showFil [8,7..1] ++
      "  ----------------- \n" ++
      "   a b c d e f g h  \n"
    where
      showFil fil = show fil ++ " " ++
                    concatMap (showCol fil) ['a'..'h'] ++ "| " ++
                    show fil ++ "\n"
      showCol fil col = "|" ++ p (tablero (col,fil))
      p Nothing = " "
      p (Just Blanco) = "B"
      p (Just Negro) = "N"

-- Ejercicio 1

vacio::Tablero
vacio = T (\x -> Nothing)

tableroInicial::Tablero
tableroInicial = T (\p ->isInicial p)

isInicial::Posicion->Maybe Color
isInicial (c,f) = if ((c,f) == ('d',4) || (c,f) == ('e',5)) then Just Negro else ( if (c,f) == ('d',5) || (c,f) == ('e',4) then Just Blanco else Nothing)

-- Ejercicio 2

contenido::Posicion -> Tablero -> Maybe Color
contenido p2 (T f) = f p2

poner::Posicion -> Color -> Tablero -> Tablero
poner p c (T f) = T (ponerAux p c f)

ponerAux::Posicion -> Color -> (Posicion -> Maybe Color) ->(Posicion -> Maybe Color)
ponerAux p c f = (\x -> if p==x then Just c else f x)


-- Ejercicio 3

desplazarFila :: Int -> Posicion -> Posicion
desplazarFila n (c,f) = (c,f+n)

desplazarColumna :: Int -> Posicion -> Posicion
desplazarColumna n (c,f) = (chr(ord(c)+n),f)

-- Ejercicio 4
--Devuelve la lista de posiciones hasta el extremo del tablero desde la posicion dada, avanzando con la funcion provista. Si la posición inicial no pertenece al tablero devuelve la lista vacía.
generar :: Posicion -> (Posicion -> Posicion ) -> [ Posicion ]
generar p d = [x | x<-(takeWhile (\p -> adentro p)(iterate d p) )]

-- Verdadera si la posición es una posicion valida del tablero.
adentro::Posicion->Bool
adentro (c,f) = oc >= ord('a') && oc <= ord('h') && f >= 1 && f <= 8
	where oc = ord(c)

-- Ejercicio 5

posicionesAInvertir :: Posicion -> Tablero -> [ Posicion ]
posicionesAInvertir p0 t = concat [ posVec (x,y) | x <- [-1,0,1], y <- [-1,0,1], vectorNoNulo (x,y) && (posValid (posVec (x,y)) (x,y))]
	where posVec = posicionesAInvertirVector p0 t
	      posValid = posicionesAInvertirValidas t

--True si el vector no es el vector nulo
vectorNoNulo :: (Int, Int) -> Bool
vectorNoNulo (a,b) = not ( a == 0 && b == 0)

-- Para ser una secuencia valida la siguiente a la ultima debe 
posicionesAInvertirValidas :: Tablero -> [Posicion] -> (Int, Int) -> Bool
posicionesAInvertirValidas (T f) ps (s,t) = null ps || not ((f (next (last ps))) == Nothing)
	where next = (desplazarColumna s).(desplazarFila t)

-- Genera todas las posiciones factibles de invertirse hacia una determinada dirección
posicionesAInvertirVector :: Posicion -> Tablero -> (Int, Int) -> [Posicion]
posicionesAInvertirVector p0 (T f) (s,t) = takeWhile (criteria p0 f) (generar (next p0) next)
	where next = (desplazarColumna s).(desplazarFila t)


--Recibe una posicion dentro del tablero, devuelve false cuando recibe una de su mismo color, error si está vacia ella o la otra
criteria:: Posicion -> (Posicion -> Maybe Color) -> Posicion -> Bool
criteria p0 f p | f p0 == Nothing = error "La casilla debe tener una ficha valida"
		| f p  == Nothing = False
		| otherwise 	  = (f p0) == invertir (f p)

-- Ejercicio 6

invertirTodas :: [ Posicion ] -> Tablero -> Tablero
invertirTodas xs (T f) =  T (\p -> if (elem p xs) then invertir (f p) else (f p))

-- El enunciado dice: Asumir que en todas las posiciones hay un ficha. Por lo que debe haber un Just color.
invertir:: Maybe Color -> Maybe Color
invertir c = case c of 
		Nothing -> error "Nothing no es inversible"
	   	Just color -> Just (invertir' color) 

--Dado un color lo invierte.
invertir':: Color -> Color
invertir' Blanco = Negro
invertir' Negro	= Blanco



-- AUXILIARES

posiciones :: [Posicion]
posiciones = [ (x,y) | x <- ['a'..'h'], y <- [1..8]]


-- devuelve que color tiene más fichas, utilizado en "ganador" de Othelo
quienTieneMas :: Tablero -> Maybe Color
quienTieneMas t	| fichasBlancas > fichasNegras = Just Blanco
		| fichasNegras > fichasBlancas = Just Negro
		| otherwise 		       = Nothing
	where 	fichasBlancas = cuantasFichas t Blanco
		fichasNegras  = cuantasFichas t Negro

--Devuelve cuantas fichas hay de un color
cuantasFichas :: Tablero -> Color -> Int
cuantasFichas (T f) c = sum [1 | pos <- posiciones , (f pos) == Just c]

--Devuelve la cantidad de fichas en en tablero
totalFichas :: Tablero -> Int
totalFichas (T f) = sum [1 | pos <- posiciones , not (contenido pos (T f) == Nothing)]
