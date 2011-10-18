
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
mover(pos(A,B), este, T) :- T = pos(X,Y), A = X, Y is B-1.


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


% EJ 7
% movimiento_valido(+Tablero, -Pieza, -Dir)


% EJ 8
% mover_pieza(+Tablero1, +Pieza, +Dir, -Tablero2)


% EJ 9
% resolver(+Tablero, -Movimientos, -TableroFinal)


% EJ 10
% armar_tablerosA(?Tablero)


% EJ 11
% armar_tablerosB(?Tablero)


