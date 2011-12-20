
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
	member(pieza(objetivo, PosPiezaObj), PiezasOrdenadas),
	quitar(pieza(objetivo, PosPiezaObj), PiezasOrdenadas, PiezasSinObjetivo),
	poner_si_valida(tablero(Dim, Pos, Piezas), pieza(objetivo, PosPiezaObj), TableroConObjetivo),
	not(member(pieza(objetivo, _), PiezasSinObjetivo)),
	/* Ubicar cada una de las piezas, mientras sea resoluble*/
	ubicar_piezas_en_tablero(TableroConObjetivo, PiezasSinObjetivo, TableroUbicado),
	resoluble(TableroUbicado),
	mostrar(TableroUbicado).
	
% resoluble(+Tablero)
% Vale si el tablero es resoluble con alguna lista de movimientos.
resoluble(T):-
	resolver(T, _, _),!.

% Devuelve el tablero recibido con las piezas instanciadas,
% generando todas las combinaciones(dentro del tablero y sin superponerse).
% ubicar_piezas_en_tablero(+Tablero, ?Piezas, -Tablero)
ubicar_piezas_en_tablero(T1, [], T1).
ubicar_piezas_en_tablero(T1, [Pieza|Piezas], T2) :-
	poner_si_valida(T1, Pieza, T3),
	ubicar_piezas_en_tablero(T3, Piezas, T2).


% pone en TF la pieza si es valida.
% poner_si_valida(+T1, ?Pieza, -TF)
poner_si_valida(tablero(Dim, PosObjetivo, PiezasOriginales), pieza(Tipo,Posicion), tablero(Dim, PosObjetivo, [pieza(Tipo,Posicion)| PiezasOriginales])):-
	en_tablero(tablero(Dim, PosObjetivo, PiezasOriginales), Posicion),
	pieza_en_tablero(tablero(Dim, PosObjetivo, PiezasOriginales), pieza(Tipo,Posicion)),
	no_superponen_entre_piezas(PiezasOriginales, pieza(Tipo,Posicion)).
%	agregar_ordenado(pieza(Tipo,Posicion), PiezasOriginales, PiezasNuevas).

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
	
/*
Tests Helper:
mover_pieza(tablero(tam(4, 4), pos(3, 3), [pieza(objetivo, pos(1, 3)), pieza(vertical, pos(3,2))]), pieza(objetivo , pos(1, 3)), sur , T). Anda OK
problema(t0, Tablero), resolver(Tablero, _, Final),mostrar(Tablero), mostrar(Final).
movimiento_valido(tablero(tam(4, 4), pos(3, 3), [pieza(objetivo, pos(1, 3)), pieza(vertical, pos(3,2))]), pieza(objetivo, pos(1, 3)), sur), mostrar(tablero(tam(4, 4), pos(3, 3), [pieza(objetivo, pos(1, 3)), pieza(vertical, pos(3,2))])).
pieza_en_tablero(tablero(tam(4, 4), pos(3, 3), [pieza(objetivo, pos(1, 3)), pieza(vertical, pos(3,2))]), pieza(objetivo, pos(1,4)))
problema(t0, T0), mover_pieza(T0, pieza(objetivo , pos(3, 1)), sur , T).
problema(t0, Tablero), resolver(Tablero, _, Final), mostrar(Tablero), mostrar(Final).
resolver(tablero(tam(2,3),pos(1,2), [pieza(objetivo, pos(1,1)), pieza(vertical, pos(1,3))]), MS, TF), mostrar(TF).

%Debería poder ser armado ya que las piezas se encuentran posicionadas de manera correcta en el tablero. 
armar_tablerosF(tablero(tam(4, 4), pos(3, 3), [pieza(objetivo, pos(1, 3)), pieza(vertical, pos(3,2))])).
armar_tablerosG(tablero(tam(4, 4), pos(3, 3), [pieza(objetivo, pos(1, 3)), pieza(vertical, pos(3,2))])).

%Debería poder ubicar las piezas con varias posibilidades
armar_tablerosF(tablero(tam(2,4),pos(1,2), [pieza(objetivo, PO), pieza(unidad, PU1), pieza(unidad, PU2)])).

%No debería armar un tablero sin objetivo. 
armar_tablerosF(tablero(tam(4, 4), pos(3, 3), [pieza(vertical, P)])).

%No debería armar un tablero con dos objetivos. 
armar_tablerosF(tablero(tam(5, 5), pos(3, 3), [pieza(objetivo, P), pieza(objetivo, Q)])).

%No debería poder ser armado ya que no entran las piezas 
armar_tablerosF(tablero(tam(3, 2), pos(1, 1), [pieza(objetivo, P), pieza(vertical, Q)])).

%Debería poder ser armado con una posibilidad. 
armar_tablerosF(tablero(tam(2, 3), pos(1, 1), [pieza(objetivo, P), pieza(vertical, Q)])).

*/

% EJ 10
% armar_tablerosF(?Tablero)
armar_tablerosF(tablero(Dim, Pos, Piezas)):-
	ubicar_piezas_en_tableroF(tablero(Dim, Pos, []), Piezas, T2),
	un_objetivo(Piezas),
	sort(Piezas,Piezas),
	resoluble(T2)	.

%Verifica que tenga una sola pieza objetivo. 
%Las piezas deben tener instanciado el tipo. 
%un_objetivo(+Piezas)
un_objetivo(Piezas):- 
	member(pieza(objetivo, PosPiezaObj), Piezas),
	quitar(pieza(objetivo, PosPiezaObj), Piezas, PiezasSinObjetivo),
	not(member(pieza(objetivo, _), PiezasSinObjetivo)).

% Devuelve el tablero recibido con las piezas instanciadas generando 
% todas las combinaciones, dentro del tablero y sin superponerse.
% ubicar_piezas_en_tableroF(+TableroInicial, ?Piezas, -TableroFinal)
ubicar_piezas_en_tableroF(TableroConTodas, [], TableroConTodas).

ubicar_piezas_en_tableroF(TableroSinPieza, [Pieza|Piezas], TableroConTodas) :-
	%Instancia la posición de la pieza en caso de ser necesario. 
	poner_si_validaF(TableroSinPieza, Pieza, TableroConPieza),
	%Realiza la recursión sobre el resto de las piezas. 
	ubicar_piezas_en_tableroF(TableroConPieza, Piezas, TableroConTodas).


%Pone en TableroFinal la pieza si es valida.
%Se encarga de instanciar la posición en caso de no estarlo. 
%poner_si_validaF(+TableroInicial, ?Pieza, -TableroFinal)
poner_si_validaF(tablero(Dim, Pos, PiezasOriginales), pieza(TipoPieza,Posicion),
	tablero(Dim, Pos, [pieza(TipoPieza,Posicion)|PiezasOriginales])):-
	%Me instancia la pieza en alguna posicion.
	en_tablero(tablero(Dim, Pos, PiezasOriginales), Posicion),
	%Chequeo que las posiciones que ocupa la pieza estén en el tablero
	pieza_en_tablero(tablero(Dim, Pos, PiezasOriginales), pieza(TipoPieza,Posicion)), 
	%Chequeo que la pieza no se superponga con ninguna otra. 
	no_superponen_entre_piezas(PiezasOriginales, pieza(TipoPieza,Posicion)).


% EJ 11
% armar_tablerosG(?Tablero)
armar_tablerosG(tablero(Dim, Pos, Piezas)):-
	ubicar_piezas_en_tableroG(tablero(Dim, Pos, []), Piezas, _).

ubicar_piezas_en_tableroG(TableroSinPieza, [Pieza|Piezas], TableroConTodas) :-
	%Instancia la posición de la pieza en caso de ser necesario. 
	poner_si_validaF(TableroSinPieza, Pieza, tablero(Tam,PosObj,PiezasTablero)),
	%Verifico lo que antes revisaba a nivel tablero final en cada momento que agrego una pieza. 
	un_objetivo(PiezasTablero),
	sort(PiezasTablero,PiezasTablero),
	resoluble(tablero(Tam,PosObj,PiezasTablero)),
	%Realiza la recursión sobre el resto de las piezas. 
	ubicar_piezas_en_tableroG(tablero(Tam,PosObj,PiezasTablero), Piezas, TableroConTodas).



% EJ 10
/* armar_tablerosC:
	Para cada tablero ingresado, se encarga de devolver todos aquellos con piezas instanciadas
	tal que cumplen con el invariante de ser un tablero válido y resoluble. Además aquellas piezas
	que estaban instanciadas se mantienen inalteradas.
*/
% armar_tablerosC(?Tablero)
armar_tablerosC(Tablero) :-
	Tablero = tablero(tam(_, _), pos(_, _), Ps),
	forall(member(pieza(_, pos(X,_)),Ps), nonvar(X)), %verifica que todas las posiciones X estan instanciadas
	forall(member(pieza(_, pos(_,Y)),Ps), nonvar(Y)), %lo mismo para Y
	sort(Ps, Ps), %se asegura que cumple con el invariante del orden
	forall(quitar(PiezaActual, Ps, PR), %verifica que no hay piesas que se pisen
		forall(member(PiezaCompara, PR),
			forall(pieza_ocupa(PiezaActual, PosFichaNueva),
				forall(pieza_ocupa(PiezaCompara, PosAux), PosAux \= PosFichaNueva)
			)
		)
	),
	resolver(Tablero, _, _), %que el tablero sea resoluble 
	!. %y con que haya una sola ya toma el tablero como váldo
armar_tablerosC(tablero(tam(Filas, Columnas), pos(XObj, YObj), Ps)) :-
	(not(forall(member(pieza(_, pos(X,_)),Ps), nonvar(X))) ;
		not(forall(member(pieza(_,pos(_,Y)),Ps), nonvar(Y)))), !, %en estas dos lineas, se fija que quede alguna piesa por resolver
	%ahora, si todavía existe alguna sin instanciar
	member(pieza(Tipo, pos(X,Y)), Ps), %para cada pieza
	(var(X) ; var(Y)), !, %filtrando solo las que tienen alguna de sus variables no instanciadas
	tamano(Tipo, tam(XTamAux, YTamAux)),
	FilasMax is Filas - XTamAux + 1,
	ColumnasMax is Columnas - YTamAux + 1,
	between(1,FilasMax,X), %si X no está instanciada, para cada x en el tablero. Si x esta instnaciada solo para la suya da true
	between(1,ColumnasMax,Y), %igual para Y
	%hasta aca nos aseguramos que no se vaya del tablero
	armar_tablerosC(tablero(tam(Filas, Columnas), pos(XObj, YObj), Ps)). %recursion

% EJ 11
/* armar_tablerosD:
	Para cada tablero ingresado, se encarga de devolver todos aquellos con piezas instanciadas
	tal que cumplen con el invariante de ser un tablero válido y resoluble. Además aquellas piezas
	que estaban instanciadas se mantienen inalteradas. La diferencia con armar_tablerosC es que
	va cumpliendo con el invariante durante el proceso de armado del tablero, haciendo la poda
	mucho mas eficiente. 
*/
% armar_tablerosD(?Tablero)
armar_tablerosD(Tablero) :- %caso base
	Tablero = tablero(tam(_, _), pos(_, _), Ps),
	forall(member(pieza(_, pos(X,_)),Ps), nonvar(X)), %si ya estan todas las posiciones X instanciadas
	forall(member(pieza(_, pos(_,Y)),Ps), nonvar(Y)), %y todas las posiciones Y instanciadas
	resolver(Tablero, _, _), !. %y el tablero final se puede resolver al menos una vez, cumple con esta regla

armar_tablerosD(tablero(tam(Filas, Columnas), pos(XObj, YObj), Ps)) :-
	(not(forall(member(pieza(_, pos(X,_)),Ps), nonvar(X))) ;
		not(forall(member(pieza(_,pos(_,Y)),Ps), nonvar(Y)))), !, %si existe alguna posicion no instanciada
	%ahora, si todavía existe alguna sin instanciar
	member(pieza(Tipo, pos(X,Y)), Ps), %para cada pieza
	(var(X) ; var(Y)), !, %filtrando solo las que tienen alguna de sus variables no instanciadas
	tamano(Tipo, tam(XTamAux, YTamAux)),
	FilasMax is Filas - XTamAux + 1,
	ColumnasMax is Columnas - YTamAux + 1,
	between(1,FilasMax,X), %si X no está instanciada, para cada x en el tablero. Si x esta instanciada solo para la suya da true
	between(1,ColumnasMax,Y), %igual para Y
	quitar(pieza(Tipo, pos(X,Y)), Ps, PR), %en estas lineas, hasta el fin del for, vemos aseguramos que las piezas no se pisen
	forall(member(PiezaCompara, PR),
		(PiezaCompara = pieza(_,pos(XAux,YAux)),
		 (var(XAux);var(YAux);
			forall(pieza_ocupa(pieza(Tipo, pos(X,Y)), PosNueva),
				(forall(pieza_ocupa(PiezaCompara, PosCompara), PosCompara \= PosNueva) )
				)
		 )
		)
	), %con esto ya no se pisan!
	armar_tablerosD(tablero(tam(Filas, Columnas), pos(XObj, YObj), Ps)), %recursión
	sort(Ps,Ps). %y aseguramos que esten ordenadas las piezas
