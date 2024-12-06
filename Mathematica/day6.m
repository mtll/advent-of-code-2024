(* ::Package:: *)

SetDirectory[NotebookDirectory[]];


(* ::Title:: *)
(*2024 Day 6*)


input=Characters@ReadList["../input/6.txt", "String"];
obs = Complex@@@Position[input, "#"];
start = Complex@@FirstPosition[input, "^"] -> -1;


floorQ[p_ -> d_] := {1, 1} \[VectorLessEqual] ReIm[p+d] \[VectorLessEqual] Dimensions[input];
obsQ[p_,d_] := MemberQ[obs, p+d];
walk[p_->d_] := If[obsQ[p, d], p -> d*-I, p+d -> d];


(* ::Subsection:: *)
(*Part 1*)


path = NestWhileList[walk, start, floorQ];
Length[<|path|>]


(* ::Subsection:: *)
(*Part 2*)


loopQ[pos_ -> dir_] := Catch@Block[{obs = Append[obs, pos+dir], obsQ},
	obsQ[p_,d_] := If[MemberQ[obs, p+d],
		(obsQ[p,d] := Throw[True]); True,
		obsQ[p,d] = False];
	NestWhile[walk, pos -> dir, floorQ];
	False];


(* ::Subsubsection:: *)
(*It's the CPU's Problem Now*)


Parallelize[loopQ /@ DeleteDuplicatesBy[path, Plus@@#&]] // Count[True]
