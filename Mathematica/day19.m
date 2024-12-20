(* ::Package:: *)

SetDirectory[NotebookDirectory[]];


(* ::Title:: *)
(*2024 Day 19*)


input = StringSplit[ReadList["../input/19.txt", "String"], ", "];
towels = input[[1]];
patterns = Flatten@input[[2;;]];


arrange[g_Graph, end_] := 0 /; GraphDistance[g, 1, end] == \[Infinity];
arrange[g_Graph, end_] := Module[{mat = AdjacencyMatrix[g]},
    Total[NestList[mat . #&, mat, end - 1][[All, 1, end]]]];
arrange[pat_] := Module[{vs, es, len = StringLength[pat] + 1},
    vs = Range[1, len];
    es = Rule@@@(StringPosition[pat, towels] + Threaded@{0,1});
    arrange[Graph[vs, es], len]]


(* ::Subsection:: *)
(*Part 1*)


Count[arrange /@ patterns, Except[0]]


(* ::Subsection:: *)
(*Part 1*)


Total[arrange /@ patterns]
