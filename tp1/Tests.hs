import Tablero
import Othello
import HUnit
import Maybe

-- evaluar t para correr todos los tests
t = runTestTT allTests

allTests = test [ 
	"triviales" ~: testsTriviales,
	"tablero" ~: testsTablero,
	"othello"  ~: testsOthello
	]

testsTriviales = test [
	1 ~=? 1,
	2 ~=? 2,
	[1,2] ~=? [1,2],
	[1,2] ~~? [2,1]
	]
	
testsTablero = test [
	_N ~=? fromJust (contenido d_4 tableroInicial),
	_N ~=? fromJust (contenido e_5 tableroInicial),
	_B ~=? fromJust (contenido e_4 tableroInicial),
	_B ~=? fromJust (contenido d_5 tableroInicial),
	Nothing ~=? contenido a_1 tableroInicial,
	"ejercicio1" ~:testsEjercicio1,
	"ejercicio2" ~:testsEjercicio2,
	"ejercicio3" ~:testsEjercicio3,
	"ejercicio4" ~:testsEjercicio4,
	"ejercicio5" ~:testsEjercicio5,
	"ejercicio6" ~:testsEjercicio6
	]
	
testsOthello = test [
	"ejercicio7" ~:testsEjercicio7,
	"ejercicio8" ~:testsEjercicio8,
	"ejercicio12" ~:testsEjercicio12
	]
	
testsEjercicio1 = test [
	[[] ~~? filter (\a -> not (contenido a vacio == Nothing) ) posiciones],
	[[d_4, e_5, e_4, d_5] ~~? filter (\a -> not (contenido a tableroInicial == Nothing) ) posiciones]
	]
	
testsEjercicio2 = test [
	_N ~=? fromJust (contenido d_6 (poner d_6 _N tableroInicial))
	]
	
testsEjercicio3 = test [
	d_1 ~=? desplazarColumna 1 ('c', 1),
	c_2 ~=? desplazarFila (-2) ('c', 4) 
	]
	
testsEjercicio4 = test [
	[f_5, f_6, f_7, f_8] ~~? generar ('f', 5) ( desplazarFila 1),
	[f_5, g_6, h_7] ~~? generar ('f', 5) ((desplazarColumna 1).( desplazarFila 1))
	]
	
testsEjercicio5 = test [
	[d_5] ~~?  posicionesAInvertir d_6 (poner d_6 Negro tableroInicial),
	[e_4] ~~? posicionesAInvertir f_3 (poner f_3 Negro(poner d_6 Negro (poner d_5 Negro tableroInicial))),
	[d_4, e_5] ~~? posicionesAInvertir f_6 (poner f_6 Blanco (poner c_3 Blanco tableroInicial)),
	[g_4] ~~? posicionesAInvertir h_4 (poner h_4 Negro (poner g_4 Blanco (poner f_4 Negro tableroInicial))),
	[d_4] ~~? posicionesAInvertir c_4 (poner c_4 Blanco (poner h_4 Negro (poner g_4 Blanco (poner f_4 Negro tableroInicial)))),
	[b_7] ~~? posicionesAInvertir a_8 (poner a_8 Blanco (poner b_7 Negro (poner c_6 Blanco (poner d_5 Negro tableroInicial)))),
	[d_2, d_3, d_4, e_2] ~~?  posicionesAInvertir d_1 tableroEjemplo,
	[] ~~? posicionesAInvertir c_1 (poner c_1 Negro (poner d_1 Blanco (poner e_1 Blanco (poner f_1 Blanco vacio))))
	]
	
testsEjercicio6 = test [
	True ~=? todasColor (Just Negro) (invertirTodas [d_5,e_4] tableroInicial),
	True ~=? todasColor (Just Blanco) (invertirTodas [d_4,e_5] tableroInicial)
	]
	
testsEjercicio7 = test [
	Just (J Blanco tableroInicial) ~=? jugar Paso inicial,
	Nothing ~=? jugar (M ('a',9::Int)) inicial,
	Nothing ~=? jugar (M d_5) inicial,
	Nothing ~=? jugar (M c_4) inicial,  -- creo que esto no pasa por el bug del ej5
	Just (J Blanco (poner c_5 Negro (poner d_5 Negro tableroInicial))) ~=? jugar (M c_5) inicial
	]

testsEjercicio8 = test [
	[(M d_6), (M c_5), (M e_3), (M f_4)] ~~? jugadasSinJuegos (J Negro tableroInicial),
	[(M c_2), (M d_1), (M e_1), (M f_1), (M g_4), (M f_5), (M e_6), (M f_6)] ~~? jugadasSinJuegos (J Blanco tableroEjemplo),
	[] ~~? jugadasSinJuegos (J Negro tableroPasaNegro),
	Paso ~=? fst (head(jugadasPosibles (J Negro tableroPasaNegro))),
	(J Blanco tableroPasaNegro) ~=? snd (head(jugadasPosibles (J Negro tableroPasaNegro))),
	[] ~~? jugadasSinJuegos (J Blanco tableroCompletoGanaBlanco),
	[] ~~? jugadasSinJuegos (J Negro tableroCompletoGanaBlanco),
	[] ~~? jugadasSinJuegos (J Blanco tableroGanaBlanco),
	[] ~~? jugadasSinJuegos (J Negro tableroGanaBlanco)
	]
	

testsEjercicio12 = test [
	Nothing ~=? ganador (J Negro tableroInicial),
	Nothing ~=? ganador (J Negro tableroPasaNegro),
	True ~=? terminoJuego (J Blanco tableroGanaBlanco),
	True ~=? terminoJuego (J Negro tableroGanaBlanco),
	Just Blanco ~=? ganador (J Blanco tableroGanaBlanco),
	True ~=? terminoJuego (J Blanco tableroCompletoGanaBlanco),
	True ~=? terminoJuego (J Negro tableroCompletoGanaBlanco),
	Just Blanco ~=? quienTieneMas  tableroCompletoGanaBlanco,
	Just Blanco ~=? ganador (J Blanco tableroCompletoGanaBlanco) 
	]
	
	
-- idem ~=? pero sin importar el orden
(~~?) :: (Ord a, Eq a, Show a) => [a] -> [a] -> Test
expected ~~? actual = (sort expected) ~=? (sort actual)
	where
		sort = foldl (\r e -> push r e) []
		push r e = (filter (e<=) r) ++ [e] ++ (filter (e>) r)

(~~) :: (Ord a, Eq a, Show a) => [a] -> [a] -> Bool
expected ~~ actual = (sort expected) == (sort actual)
	where
		sort = foldl (\r e -> push r e) []
		push r e = (filter (e<=) r) ++ [e] ++ (filter (e>) r)

-- constantes para que los tests sean más legibles		
_N = Negro
_B = Blanco
a_1 = ('a',1::Int)
b_1 = ('b',1::Int)
c_1 = ('c',1::Int)
d_1 = ('d',1::Int)
e_1 = ('e',1::Int)
f_1 = ('f',1::Int)
g_1 = ('g',1::Int)
h_1 = ('h',1::Int)

a_2 = ('a',2::Int)
b_2 = ('b',2::Int)
c_2 = ('c',2::Int)
d_2 = ('d',2::Int)
e_2 = ('e',2::Int)
f_2 = ('f',2::Int)
g_2 = ('g',2::Int)
h_2 = ('h',2::Int)

a_3 = ('a',3::Int)
b_3 = ('b',3::Int)
c_3 = ('c',3::Int)
d_3 = ('d',3::Int)
e_3 = ('e',3::Int)
f_3 = ('f',3::Int)
g_3 = ('g',3::Int)
h_3 = ('h',3::Int)

a_4 = ('a',4::Int)
b_4 = ('b',4::Int)
c_4 = ('c',4::Int)
d_4 = ('d',4::Int)
e_4 = ('e',4::Int)
f_4 = ('f',4::Int)
g_4 = ('g',4::Int)
h_4 = ('h',4::Int)

a_5 = ('a',5::Int)
b_5 = ('b',5::Int)
c_5 = ('c',5::Int)
d_5 = ('d',5::Int)
e_5 = ('e',5::Int)
f_5 = ('f',5::Int)
g_5 = ('g',5::Int)
h_5 = ('h',5::Int)

a_6 = ('a',6::Int)
b_6 = ('b',6::Int)
c_6 = ('c',6::Int)
d_6 = ('d',6::Int)
e_6 = ('e',6::Int)
f_6 = ('f',6::Int)
g_6 = ('g',6::Int)
h_6 = ('h',6::Int)

a_7 = ('a',7::Int)
b_7 = ('b',7::Int)
c_7 = ('c',7::Int)
d_7 = ('d',7::Int)
e_7 = ('e',7::Int)
f_7 = ('f',7::Int)
g_7 = ('g',7::Int)
h_7 = ('h',7::Int)

a_8 = ('a',8::Int)
b_8 = ('b',8::Int)
c_8 = ('c',8::Int)
d_8 = ('d',8::Int)
e_8 = ('e',8::Int)
f_8 = ('f',8::Int)
g_8 = ('g',8::Int)
h_8 = ('h',8::Int)

--Es el tablero de ejemplo de la figura 2 del enunciado
tableroEjemplo = (poner d_1 Blanco (poner f_2 Negro (poner e_2 Negro (poner d_2 Negro (poner f_4 Negro (poner f_3 Blanco (poner g_3 Blanco (poner e_3 Negro (poner d_3 Negro (poner c_3 Blanco (poner c_4 Blanco tableroInicial)))))))))))

-- El negro no puede jugar en este tablero
tableroPasaNegro = (poner b_2 Blanco (poner c_2 Blanco (poner a_2 Blanco (poner d_2 Negro (vacio)))))

-- Tablero final gana el blanco
tableroGanaBlanco = (poner b_2 Blanco (poner c_2 Blanco (poner a_2 Blanco (poner d_2 Blanco (vacio)))))

-- Tablero completo con mayor cant de fichas blancas que negras
tableroCompletoGanaBlanco = generarTableroGanaBco


--auxiliares

-- verifica que todas las pocisiones llenas sean de un color
todasColor :: Maybe Color -> Tablero -> Bool
todasColor c (T f)= all(\x -> contenido x(T f) ==  c || contenido x (T f) == Nothing ) posiciones

--genera tablero completo en el que ganan las blancas por cantidad
generarTableroGanaBco::Tablero
generarTableroGanaBco = T (\p -> completoGanaBco p)

completoGanaBco::Posicion->Maybe Color
completoGanaBco (c,f) = if f<6 then Just Blanco else Just Negro


inicial :: Juego
inicial = J Negro tableroInicial
