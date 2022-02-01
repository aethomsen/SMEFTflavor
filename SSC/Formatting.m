(* ::Package:: *)

Package["SSC`"] 


(* ::Title:: *)
(*SSC`Formatting*)


(* ::Subtitle:: *)
(*Contains formatting information for output*)


(* ::Section:: *)
(*Scoping & usage definitions*)


PackageExport["OpForm"]


PackageScope["SetIndexedObject"]


(* ::Section:: *)
(*OpForm*)


(* ::Subsection:: *)
(*Properties*)


(* ::Text:: *)
(*Defines new output form*)


ParentForm[OpForm]^= StandardForm;
AppendTo[$BoxForms, OpForm];
OpForm/: Print[A___, OpForm[arg_], B___] := Print[A, Format[arg, OpForm], B]


OpForm/: MakeBoxes[a:(_Symbol |_Interger| _String), OpForm]:= FormBox[RowBox@ {ToString[a]},StandardForm];


Unprotect@String;
Format[Bar@ f_String@ inds__Index, OpForm]:= UpDownIndices[OverBar@ f, inds];
Format[f_String@ inds__Index, OpForm]:= UpDownIndices[f, inds];
(*OpForm/: MakeBoxes[f_String[inds__Index], OpForm]:= RowBox@ {UpDownIndices[f, inds]};*)
Protect@ String;


Format[Bar@ y_, OpForm]:= OverBar@ y;
Format[Index[_, d_], OpForm]:= d;
Format[Operator@y__, OpForm]:= "Operator"@ FormatOperatorIndices@ Operator@ y;


Format[PlusHc@ op_, OpForm]:= Format[#, OpForm]&/@ DisplayForm@ RowBox[{"(", op, " + H.c. ", ")"}]


(* ::Subsection:: *)
(*Objects which carry indices *)


(* ::Text:: *)
(*Ensures that a symbol is printed with up-down indices *)


SetIndexedObject@ f_Symbol:= Block[{},
	Format[f[inds__Index], OpForm]:= UpDownIndices[f, inds];
	Format[Bar@f[inds__Index], OpForm]:= UpDownIndices[Bar@ f, inds];
];
SetIndexedObject@ l_List:= (SetIndexedObject/@ l; );


SetIndexedObject@{\[CurlyEpsilon], T}


(* ::Text:: *)
(*Printing indices*)


UpDownIndices[label_,indices__]:=
	Which[
		FreeQ[{indices}, Index[_Bar, _]],
			Superscript[label,
				SubscriptStyle@ Row[Format[#, OpForm]&/@ {indices}]],
		FreeQ[{indices}, Index[Except[_Bar], _]],
			Subscript[label, 
				SubscriptStyle@ Row[Format[#, OpForm]&/@ {indices}]],
		  True,
			Subsuperscript[label,
				Row[Format[#, OpForm]&/@ Cases[{indices}, Index[_Bar, _]]],
					Row[Format[#, OpForm]&/@ Cases[{indices}, Index[Except[_Bar], _]]] 
		]
];
SubscriptStyle@ x_:= Style[x, FontSize-> 12];


(* ::Subsection:: *)
(*Operator form*)


indAlphabet= Alphabet[];
FormatOperatorIndices@ op_Operator:= Module[{inds},
	inds= DeleteDuplicates@ Cases[op, Index[_, lab_]:> lab, All];
	List@@ op/. Thread@ Rule[inds, indAlphabet[[;;Length@ inds]]]
]
