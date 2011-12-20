
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
mover(pos(A,B), norte, pos(X,B)) :- X is A-1.
mover(pos(A,B), sur, pos(X,B)) :-  X is A+1.
mover(pos(A,B), este, pos(A,Y)) :- Y is B+1.
mover(pos(A,B), oeste, pos(A,Y)) :-  Y is B-1.


% EJ 4
% en_tablero(+Tablero, ?Pos)
en_tablero(tablero(tam(A,B),_, _),pos(X,Y)):-  between(1, A, X), between(1, B, Y).

% EJ 5
% pieza_ocupa(+Pieza, -Pos)
pieza_ocupa(pieza(Tipo, pos(A,B)), pos(X,Y)) :- tamano(Tipo,tam(H,V)), MAXA is A + H - 1, MAXB is B+V-1, between(A,MAXA,X), between(B, MAXB, Y).

% EJ 6
% quitar(?X, +L, -R)
quitar(A,L,LsinA):- append(L1,[A|L2],L), append(L1,L2,LsinA).

% EJ 7
% movimiento_valido(+Tablero, -Pieza, -Dir)
movimiento_valido(tablero(T,O,Piezas), pieza(TipoPieza, PosVieja), Dir):- 
% #Pieza es una pieza del tablero
	member(pieza(TipoPieza, PosVieja),Piezas),
% #Pieza se puede mover en dicha direccion
	movimiento_posible(TipoPieza, Dir),
	mover(PosVieja, Dir, PosNueva),
% #Su nueva posicion esta en el tablero
	pieza_en_tablero(tablero(T,O,Piezas), pieza(TipoPieza, PosNueva)),
% #Su nueva posicion no se superpone con otra pieza
	quitar(pieza(TipoPieza, PosVieja), Piezas, DemasPiezas),
	no_superponen_entre_piezas(DemasPiezas, pieza(TipoPieza, PosNueva)).

% pieza_en_tablero(+T, +PiezaNueva)
pieza_en_tablero(T, PiezaNueva):-
	forall(pieza_ocupa(PiezaNueva, PosNuevaOcupada), en_tablero(T, PosNuevaOcupada)).

% #Espera que PiezaNueva no este en piezas
% no_superponen_entre_piezas(+Piezas, +PiezaNueva)
no_superponen_entre_piezas(Piezas, PiezaNueva):-
	forall( member(P,Piezas), forall(pieza_ocupa(P, PosOcupaP),forall( pieza_ocupa(PiezaNueva, PosOcupa), PosOcupaP \= PosOcupa))).
	

% EJ 8
% mover_pieza(+Tablero1, +Pieza, +Dir, -Tablero2)
mover_pieza(tablero(Tam, PosObjetivo, PiezasInicial), pieza(Tip, Pos), Direccion, tablero(Tam, PosObjetivo, PiezasFinal)):- 
	quitar(pieza(Tip, Pos), PiezasInicial, PiezasInicialSinPieza),
	mover(Pos, Direccion, NuevaPos),
	agregar_ordenado(pieza(Tip, NuevaPos), PiezasInicialSinPieza, PiezasFinal).
	

agregar_ordenado(Elemento, Lista, ResultadoOrdenado):- sort([Elemento|Lista], ResultadoOrdenado).

% EJ 9
% resolver(+Tablero, -Movimientos, -TableroFinal)
resolver(Tablero, Movimientos, TableroFinal) :- resolverParametro(Tablero, [], Movimientos, TableroFinal).
 
% resolverParametro(+TableroInicial, +TablerosViejos, -Movimientos, -TableroFinal) 
resolverParametro(tablero(Tam, Pos,Piezas),_, [], tablero(Tam, Pos,Piezas)):- member(pieza(objetivo, Pos), Piezas).

resolverParametro(tablero(Tam, Pos,Piezas), TablerosViejos,  [(Pieza, Dir)|Movimientos], TableroFinal):- 
	not(member(pieza(objetivo, Pos), Piezas)),
	not(member(tablero(Tam, Pos,Piezas), TablerosViejos)),
	movimiento_valido(tablero(Tam, Pos,Piezas), Pieza, Dir), 
	mover_pieza(tablero(Tam, Pos,Piezas), Pieza, Dir, TableroIteracion), 
	resolverParametro(TableroIteracion, [tablero(Tam, Pos,Piezas) | TablerosViejos] , Movimientos, TableroFinal).

% EJ 10
% armar_tablerosA(?Tablero)
armar_tablerosA(tablero(Dim, Pos, Piezas)):-
	/* Ordenamos por orden lexicografico */
	sort(Piezas,PiezasOrdenadas),
	/* Primero acomodamos la objetivo y chequeamos que haya solo una: */
	quitar(pieza(objetivo, PObj), PiezasOrdenadas, SinObjetivo),
	poner_si_valida(tablero(Dim, Pos, Piezas), pieza(objetivo, PObj), NuevoT),
	not(member(pieza(objetivo, _), SinObjetivo)),
	/* Ubicar cada una de las piezas, mientras sea resoluble*/
	ubicar_piezas_en_tablero(NuevoT, SinObjetivo, T2),
	resoluble(T2),
	mostrar(T2).
	
% resoluble(+Tablero)
% Vale si el tablero es resoluble con alguna lista de movimientos.
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
poner_si_valida(tablero(Dim, Pos, PiezasOriginales), pieza(Tipo,Posicion), tablero(Dim, Pos, PiezasNuevas)):-
	en_tablero(tablero(Dim, Pos, PiezasOriginales), Posicion),
	pieza_en_tablero(tablero(Dim, Pos, PiezasOriginales), pieza(Tipo,Posicion)),
	no_superponen_entre_piezas(PiezasOriginales, pieza(Tipo,Posicion)),
	agregar_ordenado(pieza(Tipo,Posicion), PiezasOriginales, PiezasNuevas).

% EJ 11
% armar_tablerosB(?Tablero)
armar_tablerosB(tablero(Dim, Pos, Piezas)):-
	ubicar_piezas_en_tableroB(tablero(Dim, Pos, []), Piezas, T2),
	mostrar(T2).


% ubicar_piezas_en_tableroB(+Tablero, -Piezas, -Tablero)
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
% armar_tablerosA(tablero(tam(4, 4), pos(3, 3), [pieza(objetivo, pos(1, 3)), pieza(vertical, pos(3,2))])).
% armar_tablerosA(tablero(tam(2,4),pos(1,2), [pieza(objetivo, O), pieza(unidad, P), pieza(unidad, P2)])).

% problema(t0, Tablero), movimiento_valido(Tablero, Pieza, Dir), mover_pieza(Tablero, Pieza, Dir, TIteracion).

