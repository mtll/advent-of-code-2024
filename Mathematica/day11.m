(* ::Package:: *)

SetDirectory[NotebookDirectory[]];


(* ::Title:: *)
(*2024 Day 11*)


input = ReadList["../input/11.txt", Number];


blk[n_, 0] = 1;
blk[n_, i_] := blk[n, i] = blk[n*2024, i-1]
blk[0, i_] := blk[0, i] = blk[1, i-1];
blk[n_ /; EvenQ[\[LeftCeiling]Log[10, n+1]\[RightCeiling]], i_] :=
	blk[n, i] = Sum[blk[m, i-1], {m, QuotientRemainder[n, \!\(\*SuperscriptBox[\(10\), FractionBox[\(\[LeftCeiling]Log[10, \ n + 1]\[RightCeiling]\), \(2\)]]\)]}]


(* ::Subsection:: *)
(*Part 1*)


Sum[blk[n, 25], {n, input}]


(* ::Subsection:: *)
(*Part 2*)


Sum[blk[n, 75], {n, input}]
