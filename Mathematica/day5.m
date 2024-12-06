(* ::Package:: *)

SetDirectory[NotebookDirectory[]];


(* ::Title:: *)
(*2024 Day 5*)


{rs, lines} = SequenceSplit[ToExpression/@Import["../input/5.txt", "Data"],{{}}];


(* ::Section:: *)
(*Less Correct*)


ord[a_, b_] := ord[a, b] = Not@MemberQ[rs, {_[b, a]}];


(* ::Subsection:: *)
(*Part 1 & 2*)


Total[If[OrderedQ[#, ord], {#[[\[LeftCeiling]Length@#/2\[RightCeiling]]], 0}, {0, Sort[#, ord][[\[LeftCeiling]Length@#/2\[RightCeiling]]]}]& /@ lines]


(* ::Section:: *)
(*More Correct*)


g = Graph[rs /. {_[a_,b_]} :> a -> b];


(* ::Subsection:: *)
(*Part 1 & 2*)


val[l_] := Module[{ord = TopologicalSort[Subgraph[g, l]], s},
	s = Permute[l, Flatten@MapIndexed[FirstPosition[ord, #1, #2]&, l]];
	If[s == l, {l[[\[LeftCeiling]Length@l/2\[RightCeiling]]], 0}, {0, s[[\[LeftCeiling]Length@s/2\[RightCeiling]]]}]]


Total[val/@lines]
