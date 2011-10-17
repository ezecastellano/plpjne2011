
%%%% Tests %%%%

%% Problemas para resolver

problema(t0,
  tablero(tam(4, 4), pos(2, 3), [
    pieza(horizontal, pos(1, 2)),
    pieza(objetivo, pos(3, 1)),
    pieza(unidad, pos(2, 1)),
    pieza(unidad, pos(2, 2)),
    pieza(unidad, pos(2, 3)),
    pieza(unidad, pos(2, 4))
  ])
).

problema(t1,
  tablero(tam(4, 4), pos(3, 1), [
    pieza(horizontal, pos(2, 1)),
    pieza(objetivo, pos(1, 3)),
    pieza(unidad, pos(1, 1)),
    pieza(unidad, pos(3, 2)),
    pieza(unidad, pos(4, 1)),
    pieza(unidad, pos(4, 4))
  ])
).

problema(t2,
  tablero(tam(3, 5), pos(1, 2), [
    pieza(horizontal, pos(3, 1)),
    pieza(objetivo, pos(2, 4)),
    pieza(unidad, pos(1, 3)),
    pieza(unidad, pos(2, 3)),
    pieza(unidad, pos(3, 3)),
    pieza(vertical, pos(1, 2))
  ])
).

problema(t3,
  tablero(tam(6, 6), pos(1, 2), [
    pieza(horizontal, pos(1, 3)),
    pieza(horizontal, pos(2, 2)),
    pieza(horizontal, pos(2, 4)),
    pieza(horizontal, pos(3, 4)),
    pieza(horizontal, pos(4, 4)),
    pieza(horizontal, pos(5, 1)),
    pieza(horizontal, pos(5, 4)),
    pieza(horizontal, pos(6, 3)),
    pieza(horizontal, pos(6, 5)),
    pieza(objetivo, pos(3, 1)),
    pieza(unidad, pos(1, 2)),
    pieza(unidad, pos(1, 5)),
    pieza(unidad, pos(3, 3)),
    pieza(unidad, pos(3, 6)),
    pieza(unidad, pos(4, 3)),
    pieza(unidad, pos(5, 3)),
    pieza(unidad, pos(6, 1)),
    pieza(unidad, pos(6, 2)),
    pieza(vertical, pos(1, 6)),
    pieza(vertical, pos(4, 6))
  ])
).

%% Problemas para armar

problema(p0, tablero(tam(4, 5), pos(2, 4), [
    pieza(vertical,pos(2,3)),
    pieza(objetivo, pos(_,_)),
    pieza(unidad,pos(1,5)),
    pieza(unidad,pos(2,4)),
    pieza(unidad,pos(3,4)),
    pieza(unidad,pos(4,5)),
    pieza(horizontal,pos(_,_)),
    pieza(horizontal,pos(_,_)),
    pieza(horizontal, pos(4,1)),
    pieza(horizontal,pos(4,3))
  ])
).

problema(p1, tablero(tam(4, 5), pos(2, 4), [
    pieza(vertical,pos(2,3)),
    pieza(objetivo, pos(_,_)),
    pieza(unidad,pos(1,5)),
    pieza(unidad,pos(2,4)),
    pieza(unidad,pos(3,4)),
    pieza(unidad,pos(4,5)),
    pieza(horizontal,pos(_,_)),
    pieza(horizontal,pos(_,_)),
    pieza(horizontal, pos(_,_)),
    pieza(horizontal,pos(_,_))
  ])
).

