
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
	ubicar_piezas_en_tableroA(tablero(Dim, Pos, []), Piezas, T2),
	un_objetivo(Piezas),
	sort(Piezas,Piezas),
	resoluble(T2).

%Verifica que tenga una sola pieza objetivo. 
%Las piezas deben tener instanciado el tipo. 
%un_objetivo(+Piezas)
un_objetivo(Piezas):- 
	member(pieza(objetivo, PosPiezaObj), Piezas),
	quitar(pieza(objetivo, PosPiezaObj), Piezas, PiezasSinObjetivo),
	not(member(pieza(objetivo, _), PiezasSinObjetivo)).

% Devuelve el tablero recibido con las piezas instanciadas generando 
% todas las combinaciones, dentro del tablero y sin superponerse.
% ubicar_piezas_en_tableroA(+TableroInicial, ?Piezas, -TableroFinal)
ubicar_piezas_en_tableroA(TableroConTodas, [], TableroConTodas).

ubicar_piezas_en_tableroA(TableroSinPieza, [Pieza|Piezas], TableroConTodas) :-
	%Instancia la posición de la pieza en caso de ser necesario. 
	poner_si_validaA(TableroSinPieza, Pieza, TableroConPieza),
	%Realiza la recursión sobre el resto de las piezas. 
	ubicar_piezas_en_tableroA(TableroConPieza, Piezas, TableroConTodas).

% resoluble(+Tablero)
% Vale si el tablero es resoluble con alguna lista de movimientos.
resoluble(T):-
	resolver(T, _, _),!.

%Pone en TableroFinal la pieza si es valida.
%Se encarga de instanciar la posición en caso de no estarlo. 
%poner_si_validaA(+TableroInicial, ?Pieza, -TableroFinal)
poner_si_validaA(tablero(Dim, Pos, PiezasOriginales), pieza(TipoPieza,Posicion),
	tablero(Dim, Pos, [pieza(TipoPieza,Posicion)|PiezasOriginales])):-
	%Me instancia la pieza en alguna posicion.
	en_tablero(tablero(Dim, Pos, PiezasOriginales), Posicion),
	%Chequeo que las posiciones que ocupa la pieza estén en el tablero
	pieza_en_tablero(tablero(Dim, Pos, PiezasOriginales), pieza(TipoPieza,Posicion)), 
	%Chequeo que la pieza no se superponga con ninguna otra. 
	no_superponen_entre_piezas(PiezasOriginales, pieza(TipoPieza,Posicion)).


% EJ 11
% Es similar al EJ 10, pero en este caso verificamos que se cumpla el invariante
% en cada paso y que sea resoluble. 
% armar_tablerosB(?Tablero)
armar_tablerosB(tablero(Dim, Pos, Piezas)):-
	%Chequeo que haya un objetivo en las piezas. 
	un_objetivo(Piezas),
	%Tomo el objetivo y comienzo a construir el tablero con este en sus piezas. 
	poner_si_validaB(tablero(Dim, Pos, []),pieza(objetivo,PosPiezaObj),TableroConObjetivo),
	quitar(pieza(objetivo,PosPiezaObj),Piezas, PiezasSinObjetivo),
	ubicar_piezas_en_tableroB(TableroConObjetivo, PiezasSinObjetivo, _),
	sort(Piezas,Piezas).

% Devuelve el tablero recibido con las piezas instanciadas generando 
% todas las combinaciones, dentro del tablero y sin superponerse.
% ubicar_piezas_en_tableroB(+TableroInicial, ?Piezas, -TableroFinal)
ubicar_piezas_en_tableroB(TableroConTodas, [], TableroConTodas).

ubicar_piezas_en_tableroB(TableroSinPieza, [Pieza|Piezas], TableroConTodas) :-
	%Instancia la posición de la pieza en caso de ser necesario. 
	poner_si_validaB(TableroSinPieza, Pieza, TableroConPieza),
	%Realiza la recursión sobre el resto de las piezas. 
	ubicar_piezas_en_tableroB(TableroConPieza, Piezas, TableroConTodas).


%Pone en TableroFinal la pieza si es valida.
%Se encarga de instanciar la posición en caso de no estarlo. 
%poner_si_validaB(+TableroInicial, ?Pieza, -TableroFinal)
poner_si_validaB(tablero(Dim, Pos, PiezasOriginales), pieza(TipoPieza,Posicion),
	tablero(Dim, Pos,PiezasOrdenadas)):-
	%Me instancia la pieza en alguna posicion.
	en_tablero(tablero(Dim, Pos, PiezasOriginales), Posicion),
	%Chequeo que las posiciones que ocupa la pieza estén en el tablero
	pieza_en_tablero(tablero(Dim, Pos, PiezasOriginales), pieza(TipoPieza,Posicion)), 
	%Chequeo que la pieza no se superponga con ninguna otra. 
	no_superponen_entre_piezas(PiezasOriginales, pieza(TipoPieza,Posicion)),
	%Chequeo que queden ordenadas en el nuevo tablero. 
	sort([pieza(TipoPieza,Posicion)|PiezasOriginales],PiezasOrdenadas),
	%Chequeo que continue siendo resoluble. 
	resoluble(tablero(Dim, Pos, PiezasOrdenadas)).

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
armar_tablerosA(tablero(tam(4, 4), pos(3, 3), [pieza(objetivo, pos(1, 3)), pieza(vertical, pos(3,2))])).

%Debería poder ubicar las piezas con varias posibilidades, presatar atención a repetidos.
armar_tablerosA(tablero(tam(2,4),pos(1,2), [pieza(objetivo, PO), pieza(unidad, PU1), pieza(unidad, PU2)])).

%No debería armar un tablero sin objetivo. 
armar_tablerosA(tablero(tam(4, 4), pos(3, 3), [pieza(vertical, P)])).

%No debería armar un tablero con dos objetivos. 
armar_tablerosA(tablero(tam(5, 5), pos(3, 3), [pieza(objetivo, P), pieza(objetivo, Q)])).

%No debería poder ser armado ya que no entran las piezas 
armar_tablerosA(tablero(tam(3, 2), pos(1, 1), [pieza(objetivo, P), pieza(vertical, Q)])).

%Debería poder ser armado con una posibilidad. 
armar_tablerosA(tablero(tam(2, 3), pos(1, 1), [pieza(objetivo, P), pieza(vertical, Q)])).
*/