(* ::Package:: *)

(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]];*)


(* ::Title:: *)
(*2024 Day 8*)


(* ::Input:: *)
(*input=Characters@ReadList["../input/8.txt","String"];*)
(*ps=Position[input,#]&/@Union@Cases[input,Except["."],{2}];*)


(* ::Input:: *)
(*validQ[i_]:={1,1}\[VectorLessEqual]i\[VectorLessEqual]Dimensions[input]*)


(* ::Subsection:: *)
(*Part 1*)


(* ::Input:: *)
(*p1[a_,b_]:=Select[{2a-b,2b-a},validQ]*)


(* ::Input:: *)
(*Length@DeleteDuplicates@Flatten[p1@@@Subsets[#,{2}]&/@ps,2]*)


(* ::Subsection:: *)
(*Part 2*)


(* ::Input:: *)
(*ray[a_,b_]:=NestWhileList[#-(b-a)&,a,validQ,1,\[Infinity],-1]*)
(*p2[a_,b_]:=Join[ray[a,b],ray[b,a]]*)


(* ::Input:: *)
(*Length@DeleteDuplicates@Flatten[p2@@@Subsets[#,{2}]&/@ps,2]*)
