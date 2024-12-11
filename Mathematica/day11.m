(* ::Package:: *)

SetDirectory[NotebookDirectory[]];


(* ::Title:: *)
(*2024 Day 11*)


input=ToExpression@StringSplit@ReadString["../input/11.txt"];


step[n_, 0] = 1;
step[n_, i_] := step[n, i] = step[n*2024, i-1]
step[0, i_] := step[0, i] = step[1, i-1];
step[n_, i_] := 
	Total[step[#,i-1]&/@QuotientRemainder[n, \!\(\*SuperscriptBox[\(10\), 
FractionBox[\(\[LeftCeiling]Log[10, \ n + 1]\[RightCeiling]\), \(2\)]]\)]] /; EvenQ[\[LeftCeiling]Log[10, n+1]\[RightCeiling]]


(* ::Subsection:: *)
(*Part 1*)


Sum[step[n, 25], {n, input}]


(* ::Subsection:: *)
(*Part 2*)


Sum[step[n, 75], {n, input}]
