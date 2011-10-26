
%% Carga los módulos test y mostrar.
?- [tests].
?- [mostrar].

% EJ 1
% tamano(?Tipo, ?Tam)

tamano(objetivo, tam(2,2)).
tamano(unidad, tam(1,1)).
tamano(horizontal, tam(1,2)).
tamano(vertical, tam(2,1)).


% EJ 2
% movimiento_posible(+Tipo, -Dir)

horizontal_direcciones(este).
horizontal_direcciones(oeste).

vertical_direcciones(norte).
vertical_direcciones(sur).

todas_direcciones(T):- vertical_direcciones(T).
todas_direcciones(T):- horizontal_direcciones(T).

movimiento_posible(objetivo, T) :- todas_direcciones(T).
movimiento_posible(unidad, T):- todas_direcciones(T).
movimiento_posible(horizontal, T) :- horizontal_direcciones(T).
movimiento_posible(vertical, T) :- vertical_direcciones(T).


% EJ 3
% mover(+Pos, +Dir, -Pos)
mover(pos(A,B), norte, T) :- T = pos(X,Y), B = Y, X is A-1.
mover(pos(A,B), sur, T) :- T = pos(X,Y), B = Y, X is A+1.
mover(pos(A,B), este, T) :- T = pos(X,Y), A = X, Y is B+1.
mover(pos(A,B), oeste, T) :- T = pos(X,Y), A = X, Y is B-1.


% EJ 4
% en_tablero(+Tablero, ?Pos)
en_tablero(tablero(tam(A,B),_, _),pos(X,Y)):-  between(1, A, X), between(1, B, Y).

% EJ 5
% pieza_ocupa(+Pieza, -Pos)
pieza_ocupa(pieza(objetivo, pos(A,B)), pos(X,Y)) :- MAXA is A+1, MAXB is B+1, between(A, MAXA, X), between(B, MAXB, Y).
pieza_ocupa(pieza(unidad, A), P) :- P = A.
pieza_ocupa(pieza(horizontal, pos(A,B)), pos(X,Y)) :- MAXB is B+1, A = X, between(B, MAXB, Y).
pieza_ocupa(pieza(vertical, pos(A,B)), pos(X,Y)) :- MAXA is A+1, between(A, MAXA, X), B=Y.

% EJ 6
% quitar(?X, +L, -R)
quitar(X,L,LsinX):- append(L1,L2A,L), L2A =[A|L2], X=A, append(L1,L2,LsinX).

% EJ 7
% movimiento_valido(+Tablero, -Pieza, -Dir)
movimiento_valido(T1, Pieza, Dir):- 
% #Pieza es una pieza del tablero
	T1 = tablero(_,_,Piezas),
	member(Pieza,Piezas),
	Pieza = pieza(TipoPieza, PosVieja),
% #Pieza se puede mover en dicha direccion
	movimiento_posible(TipoPieza, Dir),
	mover(PosVieja, Dir, PosNueva),
% #Su nueva posicion esta en el tablero
	PiezaNueva = pieza(TipoPieza, PosNueva),
	pieza_en_tablero(T1, PiezaNueva),
% #Su nueva posicion no se superpone con otra pieza
	quitar(Pieza, Piezas, DemasPiezas),
	no_superponen_entre_piezas(DemasPiezas, PiezaNueva).

% pieza_en_tablero(+T, +PiezaNueva)
pieza_en_tablero(T, PiezaNueva):-
	forall(pieza_ocupa(PiezaNueva, PosNuevaOcupada), en_tablero(T, PosNuevaOcupada)).

% #Espera que PiezaNueva no este en piezas
% no_superponen_entre_piezas(+Piezas, +PiezaNueva)
no_superponen_entre_piezas(Piezas, PiezaNueva):-
	forall( member(P,Piezas), forall(pieza_ocupa(P, PosOcupaP),forall( pieza_ocupa(PiezaNueva, PosOcupa), PosOcupaP \= PosOcupa))).
	

% EJ 8
% mover_pieza(+Tablero1, +Pieza, +Dir, -Tablero2)
mover_pieza(T1, P, D, T2):-
	T1 = tablero(Tam, PosObjetivo, Piezas1),
	moverPieza(Piezas1, P, D, Piezas2),
	T2 = tablero(Tam, PosObjetivo, Piezas2).

moverPieza(PiezasInicial, Pieza , Direccion, PiezasFinal):-
	quitar(Pieza, PiezasInicial, PiezasInicialSinPieza),
	Pieza = pieza(Tip, Pos), 
	mover(Pos, Direccion, NuevaPos),
	NuevaPieza = pieza(Tip, NuevaPos),
	agregar_ordenado(NuevaPieza, PiezasInicialSinPieza, PiezasFinal).

agregar_ordenado(Elemento, Lista, ResultadoOrdenado):- sort([Elemento|Lista], ResultadoOrdenado).

% EJ 9
% resolver(+Tablero, -Movimientos, -TableroFinal)
resolver(Tablero, Movimientos, TableroFinal) :- resolverParametro(Tablero, [], Movimientos, TableroFinal).
 
% resolverParametro(+TableroInicial, +TablerosViejos, +Movimientos, +TableroInicial) 
resolverParametro(TableroInicial, _, [], TableroInicial):- 
	TableroInicial = tablero(_, Pos,Piezas), 
	member(pieza(objetivo, Pos), Piezas).
	
resolverParametro(TableroInicial, TablerosViejos, [(Pieza, Dir)| Movimientos], TableroFinal):- 
	not(member(TableroInicial, TablerosViejos)),
	movimiento_valido(TableroInicial, Pieza, Dir), 
	mover_pieza(TableroInicial, Pieza, Dir, TableroIteracion), 
	resolverParametro(TableroIteracion, [TableroInicial |TablerosViejos], Movimientos, TableroFinal).
	

% EJ 10
% armar_tablerosA(?Tablero)
armar_tablerosA(tablero(Dim, Pos, Piezas)):-
	ubicar_piezas_en_tablero(tablero(Dim, Pos, []), Piezas, T2),
	resoluble(T2),
	mostrar(T2).
	

% resoluble(+Tablero)
resoluble(T):-
	resolver(T, _, _),!.

% devuelve el tablero recibido con las piezas instanciadas.
% generando todas las combinaciones
% (dentro del tablero y sin superponerse)
% ubicar_piezas_en_tablero(+Tablero, ?Piezas, -Tablero)
ubicar_piezas_en_tablero(T1, [], T1).
ubicar_piezas_en_tablero(T1, [Pieza|Piezas], T2) :-
	poner_si_valida(T1, Pieza, T3),
	ubicar_piezas_en_tablero(T3, Piezas, T2).

% pone en TF la pieza si es valida.
% poner_si_valida(+T1, ?Pieza, -TF)
poner_si_valida(T1, Pieza, TF):-
	T1 = tablero(Dim, Pos, PiezasOriginales),
	Pieza = pieza(_,Posicion),
	en_tablero(T1, Posicion),
	pieza_en_tablero(T1, Pieza),
	no_superponen_entre_piezas(PiezasOriginales, Pieza),
	TF = tablero(Dim, Pos, [Pieza|PiezasOriginales]).

% EJ 11
% armar_tablerosB(?Tablero)
armar_tablerosB(tablero(Dim, Pos, Piezas)):-
	ubicar_piezas_en_tableroB(tablero(Dim, Pos, []), Piezas, T2),
	mostrar(T2).


% ubicar_piezas_en_tableroB(+Tablero, +Piezas, -Tablero)
ubicar_piezas_en_tableroB(T1, [], T1).
ubicar_piezas_en_tableroB(T1, [Pieza|Piezas], T2) :-
	poner_si_valida(T1, Pieza, T3),
	resoluble(T3),
	ubicar_piezas_en_tableroB(T3, Piezas, T2).
	

% Tests:
% mover_pieza(tablero(tam(4, 4), pos(3, 3), [pieza(objetivo, pos(1, 3)), pieza(vertical, pos(3,2))]), pieza(objetivo , pos(1, 3)), sur , T). Anda OK
% problema(t0, Tablero), resolver(Tablero, _, Final),mostrar(Tablero), mostrar(Final).
% movimiento_valido(tablero(tam(4, 4), pos(3, 3), [pieza(objetivo, pos(1, 3)), pieza(vertical, pos(3,2))]), pieza(objetivo, pos(1, 3)), sur), mostrar(tablero(tam(4, 4), pos(3, 3), [pieza(objetivo, pos(1, 3)), pieza(vertical, pos(3,2))])).
% pieza_en_tablero(tablero(tam(4, 4), pos(3, 3), [pieza(objetivo, pos(1, 3)), pieza(vertical, pos(3,2))]), pieza(objetivo, pos(1,4)))
% problema(t0, T0), mover_pieza(T0, pieza(objetivo , pos(3, 1)), sur , T).
% problema(t0, Tablero), resolver(Tablero, _, Final), mostrar(Tablero), mostrar(Final).
% resolver(tablero(tam(2,3),pos(1,2), [pieza(objetivo, pos(1,1)), pieza(vertical, pos(1,3))]), MS, TF), mostrar(TF).
% armar_tablerosA(tablero(tam(4, 4), pos(3, 3), [pieza(objetivo, pos(1, 3)), pieza(vertical, pos(3,2))]), pieza(objetivo , pos(1, 3))).
% armar_tablerosA(tablero(tam(2,4),pos(1,2), [pieza(objetivo, O), pieza(unidad, P), pieza(unidad, P2)])).

