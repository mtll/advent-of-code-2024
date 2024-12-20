(* ::Package:: *)

SetDirectory[NotebookDirectory[]];


(* ::Title:: *)
(*2024 Day 19*)


input = StringSplit[ReadList["../input/19.txt", "String"], ", "];
towels = input[[1]];
patterns = Flatten@input[[2;;]];


arrange[""] = 1;
arrange[str_] := arrange[str] = 
    Fold[#1 + arrange[StringTrim[str, StartOfString~~#2]]&,
         0, Select[towels, StringStartsQ[str, #]&]];


(* ::Subsection:: *)
(*Part 1*)


Count[arrange /@ patterns, Except[0]]


(* ::Subsection:: *)
(*Part 1*)


Total[arrange /@ patterns]
