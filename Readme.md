# Solver for str8ts

## Goal

Just an exercise to refresh a little bit Prolog knowledge.

The solver is for the str8ts riddle like the ones in [Sueddeutsche Zeitung](https://www.sueddeutsche.de/raetsel/str8ts).

The `solve/1` predicate takes an argument with a list of N rows,
each row contains N elements. An element has either the form `w/D` for a white cell with
digit `D` or `b/D` for a black cell with digit `D`.
Empty black cells have the form `b/0`.
Black cells are expected to be ground.

The implementation contains an example at the end of the file.

## Implementation

Most of the code is setting up the layout and extracting the streets (adjacent white cells).

The overall used technique is defining constraints with clp/fd (basically just `all_distinct/1` and basic operations like `min/2`, `#=`, `#>`, `#<`).
