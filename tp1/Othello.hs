module Othello where

import Maybe
import Tablero
import Char
import List

data Juego = J Color Tablero
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
jugar (M p) (J c t) = if (adentro p) && (vacia p t) && (rodee p t c) then Just (J (invertir' c) (realizarJugada p t c)) else Nothing

vacia:: Posicion -> Tablero -> Bool
vacia p t = (contenido p t == Nothing)

rodee::Posicion -> Tablero -> Color -> Bool
rodee p t c = not ((posicionesAInvertir p (poner p (invertir' c) t))  == [])

realizarJugada:: Posicion -> Tablero -> Color -> Tablero
realizarJugada p t c = (invertirTodas (posicionesAInvertir p (poner p (invertir' c) t)) (poner p (invertir' c) t))

-- Ejercicio 8

jugadasPosibles :: Juego -> [(Jugada,Juego)]
jugadasPosibles j = if ( genericLength (posiblesBuenas j) == 0) then [(Paso,j)] else (posiblesBuenas j)

posiblesBuenas :: Juego -> [(Jugada,Juego)]
posiblesBuenas j = [ ((M (c1,f1)),fromJust (jugar (M (c1,f1)) j)) | c1<-['a'..'h'], f1<-[1..8], not ( isNothing (jugar (M (c1,f1)) j) )]

-- Ejercicio 9

foldArbol :: (a->[b]->b) -> Arbol a -> b
foldArbol f (Nodo a ts) = f a (map (foldArbol f) ts)

-- Ejercicio 10 

podar :: Int -> Arbol a -> Arbol a
podar n a = foldArbol (\x xs -> podar' (Nodo x xs) (n-1)) a

podar' :: Arbol a -> Int -> Arbol a
podar' (Nodo a xs) n  =  if n > 0 && not( genericLength xs == 0) then Nodo a (map (\x ->podar' x (n-1)) xs)  else (Nodo a [])
 