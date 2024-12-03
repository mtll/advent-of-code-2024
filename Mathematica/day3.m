(* ::Package:: *)

SetDirectory[NotebookDirectory[]];


(* ::Title:: *)
(*2024 Day 3*)


in = ReadString["../input/3.txt"];


mul[s_] := StringCases[s, "mul("~~(x:DigitCharacter..)~~","~~(y:DigitCharacter..)~~")" -> {x,y}] //
	ToExpression // Transpose // Apply[Dot]


(* ::Subsubsection:: *)
(*Part 1*)


mul[in]


(* ::Subsubsection:: *)
(*Part 2*)


Total[mul /@ StringSplit[StringSplit[in, "do()"], "don't()"][[All,1]]]
