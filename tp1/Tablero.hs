module Tablero where

import Char
import Maybe

data Color = Blanco | Negro deriving (Show, Eq, Ord)
type Posicion = (Char, Int) 
data Tablero = T (Posicion -> Maybe Color)

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

generar :: Posicion -> (Posicion -> Posicion ) -> [ Posicion ]
generar p d = [x | x<-(takeWhile (\p -> adentro p)(iterate d p) )]

adentro::Posicion->Bool
adentro (c,f) = not ([(c1,f1) | c1<-['a'..'h'], f1<-[1..8], (c1,f1)==(c,f)] == [])

-- Ejercicio 5

posicionesAInvertir :: Posicion -> Tablero -> [ Posicion ]
posicionesAInvertir p0 (T f) = [z | s <- [1,-1], z <- (takeWhile (\x -> criteria p0 x f) (generar p0 (\x -> desplazarFila s x))), not (z == p0)] ++ [z | s <- [1,-1], z <- (takeWhile (\x -> criteria p0 x f) (generar p0 (\x -> desplazarColumna s x))), not (z == p0)]

criteria:: Posicion -> Posicion -> (Posicion -> Maybe Color) -> Bool
criteria p0 p f = (adentro p) && ((f p0) == invertir (f p) || p==p0) 

-- Ejercicio 6

invertirTodas :: [ Posicion ] -> Tablero -> Tablero
invertirTodas xs (T f) =  T (\p -> if (pertenece p xs) then invertir (f p) else (f p))

invertir:: Maybe Color -> Maybe Color
invertir c = if c == Nothing then Nothing else if c == Just Blanco then Just Negro else Just Blanco

invertir':: Color -> Color
invertir' c = if c == Blanco then Negro else Blanco

pertenece:: Eq a => a -> [a] -> Bool
pertenece y xs = not ([x | x<-xs, y==x] == [])

-- BonusTrack

isVacio::Tablero -> Bool
isVacio t = not ([(c,f) | c<-['a'..'h'], f<-[1..8], (contenido (c,f) t) == Nothing ] == []) 
