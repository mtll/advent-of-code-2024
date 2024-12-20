(* ::Package:: *)

SetDirectory[NotebookDirectory[]];


(* ::Title:: *)
(*2024 Day 19*)


input = StringSplit[ReadList["../input/19.txt", "String"], ", "];
towels = input[[1]];
patterns = Flatten@input[[2;;]];


arrange[g_Graph] := 0 /; GraphDistance[g, 1, VertexCount[g]] == \[Infinity];
arrange[g_Graph] := Module[{mat = AdjacencyMatrix[g]},
    Total[NestList[mat . #&, mat, Length[mat] - 1][[All, 1, -1]]]];
arrange[pat_String] := Module[{vs = Range[1, StringLength[pat] + 1]},
    arrange[Graph[vs, Rule@@@(StringPosition[pat, towels] + Threaded@{0, 1})]]]


(* ::Subsection:: *)
(*Part 1*)


Count[arrange /@ patterns, Except[0]]


(* ::Subsection:: *)
(*Part 1*)


Total[arrange /@ patterns]
