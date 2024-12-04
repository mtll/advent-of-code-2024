(* ::Package:: *)

SetDirectory[NotebookDirectory[]];


(* ::Title:: *)
(*2024 Day 4*)


in = Characters@ReadList["../input/4.txt", "String"];


(* ::Subsubsection:: *)
(*Part 1*)


countXmas[l_] := SequenceCount[l, Characters@"XMAS"|Characters@"SAMX", Overlaps->All]
diag[m_] := Table[Diagonal[m, n], {n, -ds, ds}]
ds = Max[Dimensions[in]] - 4;


Total[countXmas/@Join[in, in\[Transpose], diag[in], diag[Reverse/@in]]]


(* ::Subsubsection:: *)
(*Part 2*)


rot[l_] := l|l\[Transpose]|Reverse[l]|Reverse/@(l\[Transpose])


Count[BlockMap[MatchQ[rot[{{"M",_,"M"}, {_,"A",_}, {"S",_,"S"}}]], in, {3, 3}, 1], True, 2]
