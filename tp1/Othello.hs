module Othello where

import Maybe
import Tablero
import Char
import List

data Juego = J Color Tablero

instance Eq Juego where
   (J c1 t1) == (J c2 t2) = (c1 == c2) && (t1 == t2)
   
data Jugada = M Posicion | Paso
  deriving (Show, Eq)

data Arbol a = Nodo a [Arbol a] deriving Show
type ArbolJugadas = Arbol ([Jugada], Juego)

type Valuacion = Juego -> Double

instance Show Juego where
  show (J turno tablero) = "\n--Juega el " ++ show turno ++ "--\n" ++ show tablero

arbolDeJugadas :: Juego -> ArbolJugadas
arbolDeJugadas j = Nodo ([], j) $ zipWith agmov movs hijos
  where agmov m (Nodo (ms, r) hs) = Nodo ((m:ms), r) (map (agmov m) hs)
        movsJuegos = jugadasPosibles j
        movs = map fst movsJuegos
        hijos = map (arbolDeJugadas . snd) movsJuegos

-- Ejercicio 7

jugar :: Jugada -> Juego -> Maybe Juego
jugar Paso (J c t) = Just (J (invertir' c) t)
jugar (M p) (J c t) = if valida (M p) (J c t) then Just (J (invertir' c) (realizarJugada p t c)) else Nothing

-- Dice si la jugada es valida o no en un juego dado.
valida :: Jugada -> Juego -> Bool
valida (M p) (J c t) = (adentro p) && (vacia p t) && (rodee p t c) 

-- Esta habria que llevarla al modulo Tablero
vacia:: Posicion -> Tablero -> Bool
vacia p t = (contenido p t == Nothing)

--Chequea que el color c en el tablero t rodee alguna ficha invertida poniendo en la posicion c
rodee::Posicion -> Tablero -> Color -> Bool
rodee p t c = not (null (posicionesAInvertir p (poner p c t)))

--Devuelve un tablero con una jugada realizada 
realizarJugada:: Posicion -> Tablero -> Color -> Tablero
realizarJugada p t c = invertirTodas (posicionesAInvertir p (poner p c t)) (poner p c t)

-- Ejercicio 8

jugadasPosibles :: Juego -> [(Jugada,Juego)]
jugadasPosibles j = if  null (posiblesBuenas j) then [(Paso,j)] else posiblesBuenas j

-- Todas las jugadas posibles con sus juegos asociados
posiblesBuenas :: Juego -> [(Jugada,Juego)]
posiblesBuenas j = [ (jug, fromJust (jugar jug j)) | jug <- jugadasSinJuegos j]

-- Todas las jugadas validas posibles dado un juego
jugadasSinJuegos :: Juego -> [Jugada]
jugadasSinJuegos j = [ M p | p <- posiciones , valida (M p) j]

-- Ninguno de los dos participantes pueden realizar jugadas
terminoJuego:: Juego -> Bool
terminoJuego (J c t) = not (null (jugadasSinJuegos (J c t)) && null (jugadasSinJuegos  (J (invertir' c) t)))

-- Ejercicio 9

foldArbol :: (a->[b]->b) -> Arbol a -> b
foldArbol f (Nodo a ts) = f a (map (foldArbol f) ts)

-- Dejo el otro esquema que dieron en clase (donde f tiene el mismo tipo de siempre)
foldArbol':: (a -> c -> b) -> ([b] -> c) -> Arbol a -> b
foldArbol' f g (Nodo a ts) = f a (g (map (foldArbol' f g) ts) )

-- Ejercicio 10 

podar :: Int -> Arbol a -> Arbol a
podar n a = foldArbol (\x xs -> podar' (Nodo x xs) (n-1)) a

podar' :: Arbol a -> Int -> Arbol a
podar' (Nodo a xs) n  =  if n > 0 then Nodo a (map (\x ->podar' x (n-1)) xs)  else (Nodo a []) --estas haciendo recursion explicita. Fijate que con esta funcion no haria falta llamar a "podar". Lo que entendí de la sugerencia es que solo hay que hacerla para darse cuenta como funciona podar, pero no hay que usarla. (PD: le saque el chequeo por vacia por que el map sobre lista vacias devuelve la lista vacia)
 
-- Ejercicio 11
{-| MINIMAX 
-- Generación del árbol de juego: Se generarán todos los nodos hasta llegar a un estado terminal. (Listo! Parametro)
-- Cálculo de los valores de la función de utilidad para cada nodo terminal. (Utilizo la función valuación) Notar que los nodos terminales son los nodos donde vale 1 o -1
-- Calcular el valor de los nodos superiores a partir del valor de los inferiores. Alternativamente se elegirán o los valores mínimos o los valores máximos representando los movimientos del jugador y del oponente, de ahí el nombre de Minimax.
-- Elegir la jugada valorando los valores que han llegado al nivel superior (el nodo actual se elegio como el que tiene maximo valor de los minimos de más abajo)
-- Resumiendo, en cada nivel se elige el máximo  o el minimo dependiendo si es el turno mio o de mi oponente. -}

--mejorJugada :: Valuacion -> ArbolJugadas -> Jugada
--mejorJugada v ab = foldArbol 



-- Ejercicio 12
ganador:: Juego -> Maybe Color
ganador (J j t) | terminoJuego (J j t) = quienTieneMas t
	        | otherwise	  = Nothing


-- Ejercicio 13
--valuacionOthelo :: Valuacion
--valuacionOthelo 



