(* ::Package:: *)

SetDirectory[NotebookDirectory[]];


(* ::Title:: *)
(*2024 Day 18*)


input = Import["../input/18.txt", "Data"];


distance[byte_] := GraphDistance[
	VertexDelete[GridGraph[{71,71}], 
			Table[FromDigits[input[[i]], 71] + 1, {i, 1, byte}]],
	1, FromDigits[{70, 70}, 71] + 1]


(* ::Subsection:: *)
(*Part 1*)


distance[1024]


(* ::Subsection:: *)
(*Part 2*)


Module[{min = 1024, max = Length[input], i = 0},
	While[i != min,
		i = Floor[(max + min) / 2];
		If[distance[i] == \[Infinity] , max = i , min = i + 1]];
	StringRiffle[input[[min]], ","]]
