
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
quitar(X,L,LsinX):- append(L1,[X|L2],L), append(L1,L2,LsinX).

% EJ 7
% movimiento_valido(+Tablero, -Pieza, -Dir)
movimiento_valido(T1, Pieza, Dir):- 
	T1 = tablero(_,_,Piezas),
	member(Pieza,Piezas),
	Pieza = pieza(TipoPieza, PosVieja), 
	movimiento_posible(TipoPieza, Dir),
	mover(PosVieja, Dir, PosNueva),
	en_tablero(T1, PosNueva),
	P= pieza(_, Pos),
	forall(member(P, Piezas), Pos \= PosNueva).
	


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
resolver(TableroInicial, [], TableroInicial):- 
	TableroInicial = tablero(_, Pos,Piezas),
	member(pieza(objetivo, Pos), Piezas).
resolver(TableroInicial, [(Pieza, Dir)| Movimientos], TableroFinal):- 
	movimiento_valido(TableroInicial, Pieza, Dir), 
	mover_pieza(TableroInicial, Pieza, Dir, TableroIteracion), 
	resolver(TableroIteracion, Movimientos, TableroFinal).
%No estoy segura de que ande bien el 9

% EJ 10
% armar_tablerosA(?Tablero)


% EJ 11
% armar_tablerosB(?Tablero)

%mover_pieza(tablero(tam(4, 4), pos(3, 3), [pieza(objetivo, pos(1, 3)), pieza(vertical, pos(3,2))]), pieza(objetivo , pos(1, 3)), sur , T). Anda OK
%problema(t0, Tablero), resolver(Tablero, _, Final),mostrar(Tablero), mostrar(Final).
