(* ::Package:: *)

SetDirectory[NotebookDirectory[]];


(* ::Title:: *)
(*2024 Day 14*)


{ps, vs} = StringCases[#, x:NumberString.. :> ToExpression[x]]& /@
	StringSplit@ReadList["../input/14.txt", "String"] // Transpose;


step[{x_, y_},{dx_, dy_}] := {Mod[x+dx, 101], Mod[y+dy, 103]}


(* ::Subsection:: *)
(*Part 1*)


safety[ps_] :=
	Count[ps, x_ /; x \[VectorLess] {50, 51}, 1] *
	Count[ps, x_ /; {50, -\[Infinity]} \[VectorLess] x \[VectorLess] {\[Infinity], 51}, 1] *
	Count[ps, x_ /; {-\[Infinity], 51} \[VectorLess] x \[VectorLess] {50, \[Infinity]}, 1] *
	Count[ps, x_ /; {50, 51} \[VectorLess] x, 1]


safety@Nest[Thread[step[#, vs]]&, ps, 100]


(* ::Subsection:: *)
(*Part 2*)


p2 = ChineseRemainder[First@Ordering[#,1]& /@ Transpose[Variance /@ NestList[Thread[step[#, vs]]&, ps, 103]],{101, 103}] - 1


ArrayPlot@SparseArray[#+1 -> 1& /@ Nest[Thread[step[#, vs]]&, ps, p2]]
