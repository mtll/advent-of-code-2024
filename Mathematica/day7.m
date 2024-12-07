(* ::Package:: *)

SetDirectory[NotebookDirectory[]];


(* ::Title:: *)
(*2024 Day 7*)


input = ReadList["../input/7.txt", "String"];
input = ToExpression@StringSplit[input, ": " | " "];


Module[{rec, result},
	rec[0, {}, _] := True;
	rec[_, _, _] := False;
	rec[val_?Positive, l:{___, a_}, ops_] :=
		Or@@Map[rec[#[val, a], l[[;;-2]], ops]&, ops];
	trueQ[ops_] := If[rec[#[[1]], #[[2;;]], ops], #[[1]], 0]&]


detimes[a_, b_] := If[Divisible[a, b], a/b, -1]


(* ::Subsection:: *)
(*Part 1*)


Total[trueQ[{Subtract, detimes}] /@ input]


(* ::Subsection:: *)
(*Part 2*)


deconcat[a_, b_] := Module[{pre, post},
	{pre,post} = QuotientRemainder[a, \!\(\*SuperscriptBox[\(10\), \(\[LeftCeiling]Log[10, b + 1]\[RightCeiling]\)]\)];
	If[b == post, pre, -1]];


Total[trueQ[{Subtract, detimes, deconcat}] /@ input]
