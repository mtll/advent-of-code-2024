(* ::Package:: *)

SetDirectory[NotebookDirectory[]];


(* ::Title:: *)
(*2024 Day 5*)


{rs, lines} = SequenceSplit[ToExpression/@Import["../input/5.txt", "Data"],{{}}];
ord[a_, b_] := ord[a, b] = Not@MemberQ[rs, {b|a}];


(* ::Subsection:: *)
(*Part 1 & 2*)


Total[If[OrderedQ[#, ord], {#[[\[LeftCeiling]Length@#/2\[RightCeiling]]], 0}, {0, Sort[#, ord][[\[LeftCeiling]Length@#/2\[RightCeiling]]]}]& /@ lines]
