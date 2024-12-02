(* ::Package:: *)

(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]];*)


(* ::Title:: *)
(*2024 Day 1*)


{i1, i2} = Import["input/1.txt", "Data"]\[Transpose];


(* ::Subsection:: *)
(*Part 1*)


Total@Abs[Subtract @@ (Sort /@ {i1, i2})]


(* ::Subsection:: *)
(*Part 2*)


i1 . Lookup[Counts[i2], i1, 0]
