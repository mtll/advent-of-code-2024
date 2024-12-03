(* ::Package:: *)

SetDirectory[NotebookDirectory[]];


(* ::Title:: *)
(*2024 Day 3*)


in = ReadString["../input/3.txt"];


eval[exp_] := Times@@ToExpression[exp]
mul[s_] := StringCases[s, "mul("~~x:NumberString~~","~~y:NumberString~~")" :> eval@{x, y}]


(* ::Subsubsection:: *)
(*Part 1*)


Total@mul[in]


(* ::Subsubsection:: *)
(*Part 2*)


Total@Flatten[mul /@ StringSplit[StringSplit[in, "do()"], "don't()"][[All,1]]]
