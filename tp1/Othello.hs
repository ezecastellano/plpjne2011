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

