(* ::Package:: *)

SetDirectory[NotebookDirectory[]];


(* ::Title:: *)
(*2024 Day 17*)


{{ra, rb, rc}, prog} = StringCases[#, x : DigitCharacter.. :> ToExpression[x]]& @
	StringSplit[ReadString["../input/17.txt"], "\n\n"];


run[ra_, rb_, rc_, prog_] :=
    Module[{a = ra, b = rb, c = rc, out = {}, combo, inst, ip = 1},
        combo[x : 0 | 1 | 2 | 3] := x;
        combo[4] := a;
        combo[5] := b;
        combo[6] := c;
        inst[0] := a = BitShiftRight[a, combo[prog[[ip - 1]]]];
        inst[1] := b = BitXor[b, prog[[ip - 1]]];
        inst[2] := b = Mod[combo[prog[[ip - 1]]], 8];
        inst[3] := If[a != 0, ip = prog[[ip - 1]] + 1];
        inst[4] := b = BitXor[b, c];
        inst[5] := AppendTo[out, Mod[combo[prog[[ip - 1]]], 8]];
        inst[6] := b = BitShiftRight[a, combo[prog[[ip - 1]]]];
        inst[7] := c = BitShiftRight[a, combo[prog[[ip - 1]]]];
        While[ip <= Length[prog],
            ip += 2;
            inst[prog[[ip - 2]]]];
        out]


(* ::Subsection:: *)
(*Part 1*)


StringRiffle[run[ra, rb, rc, prog], ","]


(* ::Subsection:: *)
(*Part 2*)


quine[prog_] := Catch@Module[{rec},
	rec[seq_, m_]:=Module[{test, res, next},
	Do[next = Append[seq, i];
	   res = run[FromDigits[next, 8], 0, 0, prog];
	   If[res[[;;m]] == prog[[-m;;]], rec[next, m + 1]]
	   ,
	   {i, 0, 7}]];
	rec[seq_, m_ /; Length[prog] < m] := Throw@FromDigits[seq, 8];
	rec[{}, 1]]


quine[prog]
