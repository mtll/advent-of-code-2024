(* ::Package:: *)

SetDirectory[NotebookDirectory[]];


(* ::Title:: *)
(*2024 Day 19*)


input = StringSplit[ReadList["../input/19.txt", "String"], ", "];
towels = input[[1]];
patterns = Flatten@input[[2;;]];


Clear[cover];
cover[c_, len_, _] := 1 /; c == len;
cover[c_, len_, is_] := cover[c, len, is] = 
    Total[cover[#, len, is]& /@ Lookup[is, c + 1, {}]]
arrange[pat_] := Merge[Rule@@@StringPosition[pat, towels], Identity] // 
    cover[0, StringLength[pat], #]&


(* ::Subsection:: *)
(*Part 1*)


Count[arrange /@ patterns, Except[0]]


(* ::Subsection:: *)
(*Part 1*)


Total[arrange /@ patterns]
