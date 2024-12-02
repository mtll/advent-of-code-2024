(* ::Package:: *)

(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]];*)


(* ::Title:: *)
(*2024 Day 2*)


(* ::Input:: *)
(*in=Import["input/2.txt","Data"]//StringSplit//ToExpression;*)


(* ::Input:: *)
(*safe[l_]:=-3\[VectorLessEqual]#\[VectorLessEqual]-1||1\[VectorLessEqual]#\[VectorLessEqual]3&[Differences[l]]*)


(* ::Subsubsection:: *)
(*Part 1*)


(* ::Input:: *)
(*Count[in,_?safe]*)


(* ::Subsubsection:: *)
(*Part 2*)


(* ::Input:: *)
(*Count[in,_?(AnyTrue[Subsets[#,Length[#]-{1,0}],safe]&)]*)
