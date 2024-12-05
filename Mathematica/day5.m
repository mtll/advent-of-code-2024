(* ::Package:: *)

SetDirectory[NotebookDirectory[]];


(* ::Title:: *)
(*2024 Day 5*)


{rules,lines} = StringSplit/@StringSplit[ReadString["../input/5.txt"], "\n\n"];
lines = ToExpression@StringSplit[lines, ","];
ord[_, _] := False
List@@@ToExpression[rules] /. {a_, b_} :> (ord[a, b] := True);
sorted = Sort[#, ord]& /@ lines;
mask = Equal@@@({sorted,lines}\[Transpose]);


mid[l_] := l[[\[LeftCeiling]Length[l]/2\[RightCeiling]]]


(* ::Subsection:: *)
(*Part 1*)


Total[mid /@ Pick[lines, mask]]


(* ::Subsection:: *)
(*Part 1*)


Total[mid /@ Pick[sorted, mask, False]]
