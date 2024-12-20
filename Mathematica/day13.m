(* ::Package:: *)

SetDirectory[NotebookDirectory[]];


(* ::Title:: *)
(*2024 Day 13*)


input = Partition[#, 3]&@
	StringCases[ReadList["../input/13.txt", "String"], 
				x:DigitCharacter.. :> ToExpression[x]];


p[{a_, b_, p_}] :=
	Quiet@MinValue[{{3, 1} . {x, y}, x*a+y*b==p}, {x, y}, Integers] /. \[Infinity] -> 0


(* ::Subsection:: *)
(*Part 1*)


Total@Flatten[p /@ input]


(* ::Subsection:: *)
(*Part 2*)


Total@Flatten[p[#+{0, 0, 10000000000000}]& /@ input]
