(* ::Package:: *)

SetDirectory[NotebookDirectory[]];


(* ::Title:: *)
(*2024 Day 4*)


in=Characters@ReadList["../input/4.txt", "String"];


(* ::Subsubsection:: *)
(*Part 1*)


countXmas[l_]:=Total@StringCount[StringJoin/@l, "XMAS"|"SAMX", Overlaps->All]


Total[countXmas/@{in, in\[Transpose],
	Table[Diagonal[in, n], {n, -Length[in],Length[in]}],
	Table[Diagonal[Reverse/@in, n],{n, -Length[in], Length[in]}]}]


(* ::Subsubsection:: *)
(*Part 2*)


rot[l_]:=l|l\[Transpose]|Reverse[l]|Reverse/@(l\[Transpose])


BlockMap[MatchQ[rot[{{"M",_,"M"}, {_,"A",_}, {"S",_,"S"}}]], in, {3, 3}, 1]//Flatten//Count[True]
