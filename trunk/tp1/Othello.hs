module Othello where

import Maybe
import Tablero
import Char
import List

data Juego = J Color Tablero
   
instance Eq Juego where
   (J c1 t1) == (J c2 t2) = (c1 == c2) && (t1 == t2)
   
data Jugada = M Posicion | Paso
  deriving (Show, Eq, Ord)

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


--Auxiliares
turnoDe :: Juego -> Color
turnoDe (J c _) = c

valor :: Arbol a -> a
valor (Nodo a _) = a

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
jugadasPosibles j = if  null (posiblesBuenas j) then [(Paso,fromJust (jugar Paso j))] else posiblesBuenas j

-- Todas las jugadas posibles con sus juegos asociados
posiblesBuenas :: Juego -> [(Jugada,Juego)]
posiblesBuenas j = [ (jug, fromJust (jugar jug j)) | jug <- jugadasSinJuegos j]

-- Todas las jugadas validas posibles dado un juego
jugadasSinJuegos :: Juego -> [Jugada]
jugadasSinJuegos j = [ M p | p <- posiciones , valida (M p) j]

-- Ninguno de los dos participantes pueden realizar jugadas
terminoJuego:: Juego -> Bool
terminoJuego (J c t) = (null (jugadasSinJuegos (J c t)) && null (jugadasSinJuegos  (J (invertir' c) t)))

-- Ejercicio 9

foldArbol :: (a->[b]->b) -> Arbol a -> b
foldArbol f (Nodo a ts) = f a (map (foldArbol f) ts)

-- Dejo el otro esquema que dieron en clase (donde f tiene el mismo tipo de siempre)
foldArbol':: (a -> c -> b) -> ([b] -> c) -> Arbol a -> b
foldArbol' f g (Nodo a ts) = f a (g (map (foldArbol' f g) ts) )

-- Ejercicio 10 

podar :: Int -> Arbol a -> Arbol a
podar = flip podar'

podar' :: Arbol a -> Int -> Arbol a
-- (a -> [(n -> Arbol a)] -> (n -> Arbol a)) -> Arbol a -> (n -> Arbol a)
podar' = foldArbol (\x fs -> (\n -> Nodo x (aplicarConMapSiNoCero fs n))) 

aplicarConMapSiNoCero :: [(Int -> Arbol a)] -> Int -> [Arbol a]
aplicarConMapSiNoCero fs n = map (aplicar (n-1)) (if n == 0 then [] else fs)

aplicar :: a -> (a -> b) -> b
aplicar a f = f a

-- Ejercicio 11
-- Busca entre las jugadas de su oponente la que mejor lo deje parado. (maxima valuacion)
-- puedo tomar head por que siempre existe al menos una jugada (Paso)
mejorJugada :: Valuacion -> ArbolJugadas -> Jugada
mejorJugada v (Nodo jj abs) = (head.fst) (jugadaMenosPeor abs v)

-- Entre todas las jugadas de mi contrincante, devuelvo el par ([Jugada], Juego) que mejor valuación me dé.
jugadaMenosPeor :: [ArbolJugadas] -> Valuacion -> ([Jugada], Juego)
jugadaMenosPeor abs v = fst (foldr maxSnd (last l) l)
	where l = [ (valor ab, (v.snd.valor) ab) | ab <- abs ]

-- toma la tupla maxima mirando por segunda componente.
maxSnd :: Ord p => (a, p) -> (a, p) -> (a, p)
maxSnd t1 = (\t2 -> if (snd t1) > (snd t2) then t1 else t2)



-- Ejercicio 12
ganador :: Juego -> Maybe Color
ganador (J j t) | terminoJuego (J j t) = quienTieneMas t
	        | otherwise	  = Nothing


-- Ejercicio 13
valuacionOthelo :: Valuacion
valuacionOthelo (J j t) | ganador (J j t) == Just j = 1
	| ganador (J j t) == Nothing = (2 * fromIntegral (cuantasFichas t j) / fromIntegral (totalFichas t)) - 1
	| otherwise = -1



