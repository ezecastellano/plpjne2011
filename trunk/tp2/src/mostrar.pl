
%%% resolverYMostrar(+NombreProblema)

resolverYMostrar(NombreProblema) :-
  problema(NombreProblema, T), mostrar(T), resolver(T, Camino, _),
  mostrarCamino(T, Camino).

mostrarCamino(_, []).
mostrarCamino(T, [(P,D)|Ms]) :-
  mover_pieza(T, P, D, T2),
  mostrar(T2),
  mostrarCamino(T2, Ms).

%%% mostrar(+Tablero)

mostrar(Tablero) :-
  tablero(tam(_, C), _, _) = Tablero,
  format("   "),
  forall(between(1, C, J),
         format(" ~d ", [J])),
  format("\n"),
  mostrarFila(Tablero, 1),
  format("\n"), !.

mostrarFila(tablero(tam(F, _), _, _), I) :- I > F.
mostrarFila(Tablero, I) :-
  tablero(tam(_, C), _, _) = Tablero,
  format(" ~d ", [I]),
  forall(between(1, C, J),
         mostrarPos(Tablero, pos(I, J))),
  format("\n"),
  I1 is I + 1,
  mostrarFila(Tablero, I1).

mostrarPos(tablero(tam(F, _), Obj, Ps), pos(I, J)) :-
  I =< F,
  member(Pieza, Ps),
  Pieza = pieza(Tipo, pos(Px, Py)),
  tamano(Tipo, tam(Tx, Ty)),
  between(1, Tx, Xi),
  between(1, Ty, Yi),
  I is Px + Xi - 1,
  J is Py + Yi - 1,
  nth0(Index, Ps, Pieza),
  Name is 97 + Index,
  print_wrap([Name], Obj, pos(I,J)), !.
mostrarPos(tablero(tam(_, _), Obj, _), pos(I, J)) :-
  print_wrap("-", Obj, pos(I,J)).

print_wrap(S, pos(Oi,Oj), pos(I,J)) :-
  Oi =< I, I < Oi + 2,
  Oj =< J, J < Oj + 2,
  format("{"), format(S), format("}"), !.
print_wrap(S, _, _) :-
  format(" "), format(S), format(" ").

