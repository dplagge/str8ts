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
  include(non_null, Line, ALine),
  maplist(get_digit, ALine, NLine),
  maplist(is_fddigit, NLine),
  all_distinct(NLine).

non_null(X) :- X \== b/0.
is_fddigit(X) :- X #> 0, X #< 10.
get_digit(_/N,N).
llength(N,List) :- length(List, N).

all_parts(Lines, Parts) :-
  maplist(parts, Lines, LParts),
  append(LParts, Parts).

parts([],[]).
parts([A|Rest], Bs) :- parts2(A, Rest, Bs).
parts2(b/_, Arest, Bs) :- parts(Arest, Bs).
parts2(w/A, Arest, [ [A|Part] | Brest ]) :-
  collect(Arest, Part, Crest),
  parts(Crest, Brest).

collect([],[],[]).
collect([A|Arest], X, Y) :- collect2(A, Arest, X, Y).
collect2(b/_, Arest, [], Arest).
collect2(w/A, Arest, [A|Brest], Crest) :- collect(Arest, Brest, Crest).

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

solve(Solution) :-
  layout(Solution, Vars, Parts),
  maplist(street, Parts),
  label(Vars).

print_solution(Solution) :- maplist(print_line, Solution).
print_line(L) :-
  maplist(get_digit, L, Digits),
  maplist(write, Digits), write('\n').

solve_and_print(Solution) :-
  solve(Solution),
  print_solution(Solution).

template :-
  solve_and_print([
    [w/_,w/_,w/_,w/_,w/_,w/_,w/_,w/_,w/_],
    [w/_,w/_,w/_,w/_,w/_,w/_,w/_,w/_,w/_],
    [w/_,w/_,w/_,w/_,w/_,w/_,w/_,w/_,w/_],
    [w/_,w/_,w/_,w/_,w/_,w/_,w/_,w/_,w/_],
    [w/_,w/_,w/_,w/_,w/_,w/_,w/_,w/_,w/_],
    [w/_,w/_,w/_,w/_,w/_,w/_,w/_,w/_,w/_],
    [w/_,w/_,w/_,w/_,w/_,w/_,w/_,w/_,w/_],
    [w/_,w/_,w/_,w/_,w/_,w/_,w/_,w/_,w/_],
    [w/_,w/_,w/_,w/_,w/_,w/_,w/_,w/_,w/_]]).

example :-
  solve_and_print([
    [b/6,b/0,w/_,w/_,b/0,b/0,w/_,w/_,b/0],
    [w/_,w/_,w/1,w/_,b/0,w/_,w/9,w/_,b/5],
    [w/_,w/_,b/0,w/_,w/_,w/_,w/_,w/_,w/_],
    [b/0,w/_,w/_,b/0,w/_,w/6,b/1,w/_,w/_],
    [b/0,w/_,w/_,w/_,w/_,w/_,w/_,w/6,b/0],
    [w/_,w/_,b/9,w/6,w/_,b/0,w/_,w/_,b/0],
    [w/_,w/3,w/_,w/_,w/_,w/_,b/0,w/_,w/_],
    [b/0,w/_,w/6,w/_,b/0,w/_,w/_,w/_,w/_],
    [b/0,w/_,w/_,b/0,b/0,w/_,w/_,b/0,b/3]]).
