:- use_module(library(clpfd)).
:- use_module(library(apply)).

solve(Solution) :-
  % Basic layout: The solution's variables and streets
  layout(Solution, Vars, Parts),
  % Apply the street constraints
  maplist(is_valid_street, Parts),
  % Label the variables (i.e. iterating if necessary)
  label(Vars).

% overall layout: NxN solution, all digits are different, streets extracted
layout(Rows, Vars, Streets) :-
  gridcontent(Rows, Cols, Vars),
  % Extract horizontal streets
  all_streets(Rows, RowStreets),
  % Extract vertical streets
  all_streets(Cols, ColStreets),
  append(RowStreets, ColStreets, Streets).

% Set up the basic rules: N rows, N columns, all variables
% in a row/column are distinct.
%
% Rows: A list of rows, each a list of elements w/D or b/D (with D being a digit)
% Cols: A list of columns, same structure as the rows
% Vars: A list of all variables
gridcontent(Rows, Cols, Vars) :-
  % quadratic form: N rows, each N cells
  length(Rows, N), maplist(llength(N), Rows),
  transpose(Rows, Cols),
  maplist(all_digits_are_different, Rows, RowDigits),
  maplist(all_digits_are_different, Cols, _ColDigits),
  append(RowDigits, Vars).

% Extract all variables and
% example:
%    input: [w/A,b/0,b/B,w/C]
%   output: [A,B,C]
all_digits_are_different(Line, NLine) :-
  include(non_null, Line, ALine),
  maplist(get_digit, ALine, NLine),
  maplist(is_fddigit, NLine),
  all_distinct(NLine).

non_null(X) :- X \== b/0.
is_fddigit(X) :- X #> 0, X #< 10.
get_digit(_/N,N).
llength(N,List) :- length(List, N).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% street extraction

all_streets(Lines, Streets) :-
  maplist(streets, Lines, LStreets),
  append(LStreets, Streets).

streets([],[]).
streets([A|Rest], Bs) :- streets2(A, Rest, Bs).
streets2(b/_, Arest, Bs) :- streets(Arest, Bs).
streets2(w/A, Arest, [ [A|Part] | Brest ]) :-
  collect(Arest, Part, Crest),
  streets(Crest, Brest).

collect([],[],[]).
collect([A|Arest], X, Y) :- collect2(A, Arest, X, Y).
collect2(b/_, Arest, [], Arest).
collect2(w/A, Arest, [A|Brest], Crest) :- collect(Arest, Brest, Crest).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% street constraints

is_valid_street(List) :-
  length(List, N),
  maxlist(List, Max),
  minlist(List, Min),
  N #= Max-Min+1.

minlist([H|T], Min) :- foldl(imin, T, H, Min).
imin(A,B,M) :- M #= min(A,B).
maxlist([H|T], Max) :- foldl(imax, T, H, Max).
imax(A,B,M) :- M #= max(A,B).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% tools

print_solution(Solution) :- maplist(print_line, Solution).
print_line(L) :-
  maplist(get_digit, L, Digits),
  maplist(write, Digits), write('\n').

solve_and_print(Solution) :-
  solve(Solution),
  print_solution(Solution).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% template and example examples

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
