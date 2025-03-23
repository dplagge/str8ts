:- use_module(library(clpfd)).
:- use_module(library(apply)).

baserules(N, Rows, Cols, Vars) :-
  length(Rows, N),maplist(llength(N), Rows),
  length(Cols, N),maplist(llength(N), Cols),
  transpose(Rows, Cols),
  maplist(alldiffs, Rows, NRows),
  maplist(alldiffs, Cols, _),
  append(NRows, Vars).

alldiffs(Line, NLine) :-
  include(nonnull, Line, NLine),
  maplist(isdigit, NLine),
  all_distinct(NLine).

nonnull(X) :- X \== 0.
isdigit(X) :- X #> 0, X #< 10.
llength(N,List) :- length(List, N).

all_parts(Lines, Parts) :-
  maplist(parts, Lines, LParts),
  append(LParts, Parts).

parts([],[]).
parts([A|Arest], Bs) :- ground(A), !, parts(Arest, Bs).
parts([A|Arest], [ [A|Part] | Brest ]) :-
  collect(Arest, Part, Crest),
  parts(Crest, Brest).

collect([],[],[]).
collect([A|Arest], [], Arest) :- ground(A), !.
collect([A|Arest], [A|Brest], Crest) :- collect(Arest, Brest, Crest).

minlist([H|T], Min) :- foldl(imin, T, H, Min).
imin(A,B,M) :- M #= min(A,B).
maxlist([H|T], Max) :- foldl(imax, T, H, Max).
imax(A,B,M) :- M #= max(A,B).

street(List) :-
  length(List, N),
  maxlist(List, Max),
  minlist(List, Min),
  N #= Max-Min+1.

layout(Rows, Vars, Parts) :-
  baserules(_, Rows, Cols, Vars),
  all_parts(Rows, RParts),
  all_parts(Cols, CParts),
  append(RParts,CParts, Parts).

solve(InRows, Solution) :-
  layout(InRows, Vars, Parts),
  Solution = InRows,
  maplist(street, Parts),
  label(Vars).

example0(Solution) :-
  InRows = [
  [_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_]
  ],
  Solution = [
  [_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,_]
  ],
  solve(InRows, Solution).

example(Solution) :-
  InRows = [
  [6,0,_,_,0,0,_,_,0],
  [_,_,_,_,0,_,_,_,5],
  [_,_,0,_,_,_,_,_,_],
  [0,_,_,0,_,_,1,_,_],
  [0,_,_,_,_,_,_,_,0],
  [_,_,9,_,_,0,_,_,0],
  [_,_,_,_,_,_,0,_,_],
  [0,_,_,_,0,_,_,_,_],
  [0,_,_,0,0,_,_,0,3]
  ],
  Solution = [
  [6,_,_,_,_,_,_,_,_],
  [_,_,1,_,_,_,9,_,5],
  [_,_,_,_,_,_,_,_,_],
  [_,_,_,_,_,6,1,_,_],
  [_,_,_,_,_,_,_,6,_],
  [_,_,9,6,_,_,_,_,_],
  [_,3,_,_,_,_,_,_,_],
  [_,_,6,_,_,_,_,_,_],
  [_,_,_,_,_,_,_,_,3]
  ],
  solve(InRows, Solution).
