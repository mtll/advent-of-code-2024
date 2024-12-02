(* ::Package:: *)

(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]];*)


(* ::Title:: *)
(*2024 Day 2*)


in = ToExpression@StringSplit@ReadList["input/2.txt", "String"];


safe[l_] := -3 \[VectorLessEqual] # \[VectorLessEqual] -1 || 1 \[VectorLessEqual] # \[VectorLessEqual] 3 &[Differences[l]]


(* ::Subsubsection:: *)
(*Part 1*)


Count[in, _?safe]


(* ::Subsubsection:: *)
(*Part 2*)


Count[in, _?(AnyTrue[Subsets[#, {Length[#] - 1}], safe]&)]
