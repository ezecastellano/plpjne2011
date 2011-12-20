%% Carga los módulos test y mostrar.
?- [tests].
?- [mostrar].

% EJ 1
/* Tamaño:
	Definimos el tamaño de la ficha según su tipo.
*/
% tamano(?Tipo, ?Tam)
tamano(objetivo, tam(2, 2)).
tamano(unidad, tam(1, 1)).
tamano(horizontal, tam(1, 2)).
tamano(vertical, tam(2, 1)).

% EJ 2
/* movimiento_posible
	Definimos los movimientos posibles según el tipo de ficha.
*/
% movimiento_posible(+Tipo, -Dir)
movimiento_posible(objetivo, norte).
movimiento_posible(objetivo, sur).
movimiento_posible(objetivo, este).
movimiento_posible(objetivo, oeste).
movimiento_posible(unidad, norte).
movimiento_posible(unidad, sur).
movimiento_posible(unidad, este).
movimiento_posible(unidad, oeste).
movimiento_posible(horizontal, este).
movimiento_posible(horizontal, oeste).
movimiento_posible(vertical, norte).
movimiento_posible(vertical, sur).


% EJ 3
/* mover
	Para cada orientación posible (norte, sur, este y oeste) cambia la
	mueve la posición de la ficha identificada con pos(X,Y) a donde corresponda.
	Para ello suma o resta uno en la coordenada correcta.
*/
% mover(+Pos, +Dir, -Pos)
mover(pos(X,Y), norte, pos(X1, Y)) :- X1 is X-1.
mover(pos(X,Y), este, pos(X, Y1)) :- Y1 is Y+1.
mover(pos(X,Y), sur, pos(X1, Y)) :- X1 is X+1.
mover(pos(X,Y), oeste, pos(X, Y1)) :- Y1 is Y-1.


% EJ 4
/* en_tablero
	Esta función se encarga de dado un tablero de dimensiones XTam, YTam devolver todas las posiciones
	pertenecientes al mismo. Es decir, las posiciones que se encuentra entre 1 y XTam y entre 1 e YTam.
*/
% en_tablero(+Tablero, ?Pos)
en_tablero(tablero(tam(XTam,YTam), _, _), pos(XRes,YRes)) :- between(1, XTam, XRes), between(1, YTam, YRes).

% EJ 5
/* pieza_ocupa
	Devuelve todas las posiciones que ocupa una pieza determinada dado su tipo y posición inicial.
	Se basa en el tipo de la pieza y su posición actual para definir las posiciones válidas a partir de estas
*/
% pieza_ocupa(+Pieza, -Pos)
pieza_ocupa(pieza(TipoPieza, pos(X, Y)) , pos(XRes, YRes)) :- 
    tamano(TipoPieza, tam(XTam, YTam)),
    XAux is X+XTam-1,
    YAux is Y+YTam-1,
    between(X, XAux, XRes),
    between(Y, YAux, YRes).


% EJ 6
/* quitar
	Quita el elemento X de la lista L y devuelve una nueva lista R sin una aparición
	del elemento X. Si no se especifica X, devuelve todas posibles listas sin algún elemento.
*/
% quitar(?X, +L, -R)
quitar(X, L, R) :- append(L1, [X|L2], L), append(L1, L2, R).

% EJ 7
/* movimiento_valido
	Devuelve todas las piezas y su dirección tal que al moverlas no se rompe el invariante del tablero.
	
*/
% movimiento_valido(+Tablero, -Pieza, -Dir)
movimiento_valido(tablero(tam(XTam, YTam), _, Ps), pieza(TipoPieza, PosRes), Dir) :- 
  quitar(pieza(TipoPieza, PosRes), Ps, PR), %para cada pieza
  movimiento_posible(TipoPieza, Dir), %para cada movimiento de la pieza sacada
  mover(PosRes, Dir, NuevaPos), %calcula la nueva posición
  %luego, para cada posicion que ocuparia la ficha, hay que ver que queda en el tablero
  forall(pieza_ocupa(pieza(TipoPieza, NuevaPos), PosFichaNueva), 
         en_tablero(tablero(tam(XTam, YTam), _, _), PosFichaNueva)),
  %para cada posicion que ocuparia la ficha, hay que ver si choca con otra
  forall(member(PiezaCompara, PR),
    forall(pieza_ocupa(pieza(TipoPieza, NuevaPos), PosFichaNueva),
      forall(pieza_ocupa(PiezaCompara, PosAux), PosAux \= PosFichaNueva)
    )
  ).
  
% EJ 8
/* mover_pieza
	Dado un tablero, una pieza y una dirección válida mueva la pieza en esa dirección
	arrojando un tablero nuevo con esa pieza movida.
*/
% mover_pieza(+Tablero1, +Pieza, +Dir, -Tablero2)
mover_pieza(tablero(tam(X,Y), PObjs, Ps), pieza(TipoPieza, Pos), Dir, tablero(tam(X,Y), PObjs, PsRes) ) :-
  quitar(pieza(TipoPieza, Pos), Ps, PTmp), %sacamos la pieza pedida de la lista de piezas
  mover(Pos, Dir, NuevaPos), %movemos la posición que representa la pieza a mover
  append([pieza(TipoPieza, NuevaPos)], PTmp, PNuevas), %Agregamos la pieza a la lista de piezas con la posición actualizada
  setof(P,member(P, PNuevas),PsRes). %Ordenamos las piezas para cumplir con el invariante.
  
% EJ 9
/* resolver
	Resuelve el tablero devolviendo una lista de movimientos que lo resuelven y como queda
	el tablero resultante.
*/
% resolver(+Tablero, -Movimientos, -TableroFinal)
resolver(T1,Ms,T2) :- resolver2(T1, [], Ms, T2). % LLamamos a la función resolver2 agregandole como parámetro la lista de tableros que ya analizamos para la rama actual

/* resolver2
	Función auxiliar de resolver. Tiene los mismos parámetros más la lista de tableros ya recorridos de la rama actual.
*/
%resolver2(+TableroEntrada, +TablerosViejos, -MovimientosARealizar, -TableroFinal) % Si la pieza objetivo está en el objetivo termina en cero movimientos.
resolver2(tablero(Tam, PosObj, Ps), _, [], tablero(Tam, PosObj, Ps)):-
  member(pieza(objetivo, PosObj), Ps).


resolver2(Tablero, TsViejos, [(Pieza,Dir)|Ms], TFinal) :-
  Tablero = tablero(tam(_, _), PObj, Ps), 
  not(member(pieza(objetivo, PObj), Ps)), %Si todavía no está resuelto...
  not(member(Tablero, TsViejos)), %Si es un tablero todavía no analizado en la rama actual
  movimiento_valido(Tablero, Pieza, Dir), % Para cada movimiento posible del tablero
  mover_pieza(Tablero, Pieza, Dir, TAux), %Movemos la pieza...
  resolver2(TAux, [Tablero|TsViejos], Ms, TFinal). %Llamamos recursivamente a la función.

% EJ 10
/* armar_tablerosA:
	Para cada tablero ingresado, se encarga de devolver todos aquellos con piezas instanciadas
	tal que cumplen con el invariante de ser un tablero válido y resoluble. Además aquellas piezas
	que estaban instanciadas se mantienen inalteradas.
*/
% armar_tablerosA(?Tablero)
armar_tablerosA(Tablero) :-
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
armar_tablerosA(tablero(tam(Filas, Columnas), pos(XObj, YObj), Ps)) :-
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
	armar_tablerosA(tablero(tam(Filas, Columnas), pos(XObj, YObj), Ps)). %recursion

% EJ 11
/* armar_tablerosB:
	Para cada tablero ingresado, se encarga de devolver todos aquellos con piezas instanciadas
	tal que cumplen con el invariante de ser un tablero válido y resoluble. Además aquellas piezas
	que estaban instanciadas se mantienen inalteradas. La diferencia con armar_tablerosA es que
	va cumpliendo con el invariante durante el proceso de armado del tablero, haciendo la poda
	mucho mas eficiente. 
*/
% armar_tablerosB(?Tablero)
armar_tablerosB(Tablero) :- %caso base
	Tablero = tablero(tam(_, _), pos(_, _), Ps),
	forall(member(pieza(_, pos(X,_)),Ps), nonvar(X)), %si ya estan todas las posiciones X instanciadas
	forall(member(pieza(_, pos(_,Y)),Ps), nonvar(Y)), %y todas las posiciones Y instanciadas
	resolver(Tablero, _, _), !. %y el tablero final se puede resolver al menos una vez, cumple con esta regla

armar_tablerosB(tablero(tam(Filas, Columnas), pos(XObj, YObj), Ps)) :-
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
	armar_tablerosB(tablero(tam(Filas, Columnas), pos(XObj, YObj), Ps)), %recursión
	sort(Ps,Ps). %y aseguramos que esten ordenadas las piezas