(* ::Package:: *)

Package["SMEFTflavor`"] 


(* ::Title:: *)
(*SMEFTflavor`Objects*)


(* ::Subtitle:: *)
(*Defines the basic objects of the program*)


(* ::Section:: *)
(*Scoping & usage definitions*)


PackageExport["Bar"]
PackageExport["CGs"]
PackageExport["Index"]
PackageExport["Operator"]
PackageExport["PlusHc"]
PackageExport["Spur"]


PackageExport["fund"]
PackageExport["adj"]
PackageExport["SU"]


PackageExport["\[CurlyEpsilon]"]
PackageExport["T"]


PackageScope["ComplexRepQ"]
PackageScope["Charge"]
PackageScope["SymBar"]


(* ::Section:: *)
(*Objects*)


(* ::Subsection:: *)
(*Bar*)


(* ::Text:: *)
(*Properties *)


Bar@ Bar@ x_:= x
Bar[x:(_Plus| _Times| _Power| _List)]:= Bar/@x;
Bar@ x_Complex:= Conjugate@ x;
Bar[x_][ind___]:= Bar@ x@ ind;


(* ::Subsubsection:: *)
(*Baring with symmetry *)


(* ::Text:: *)
(*Barring Spurions depending on a flavor symmetry *)


SymBar[flavorSym_]@ expr_:= Module[{reals, out= Bar@ expr},
	reals= Lookup[$flavorSymmetries[flavorSym], SelfConjugate, {}];
	out/. spurs_Spur:> (spurs/. Bar@ sp_/; MemberQ[reals, sp| Head@ sp]-> sp)  
]


(* ::Subsection:: *)
(*Groups*)


(* ::Text:: *)
(*Indices*)


Bar@ Index@ rep_:= Index@ Bar@ rep;
Bar@ Index[rep_, dummy_]:= Index[Bar@ rep, dummy];


(* ::Text:: *)
(*Representations*)


GroupRepComplexQ[SU@3, fund]= True;
GroupRepComplexQ@ __= False; 
ComplexRepQ[rep_, flavorSym_]:= GroupRepComplexQ[
	$flavorSymmetries[flavorSym, Groups, Head@ rep], First@ rep];


Bar@ gr_@ adj:= gr@ adj;


(* ::Subsubsection:: *)
(*Clebsch-Gordan coefficients*)


Protect[\[CurlyEpsilon], T]


(* ::Subsection:: *)
(*Spur *)


SetAttributes[Spur, Orderless];
Bar@ x_Spur:= Bar/@ x;


(* ::Subsection:: *)
(*CGs*)


SetAttributes[CGs, Orderless];
Bar@ x_CGs:= Bar/@ x;


(* ::Subsection:: *)
(*Charge*)


(* ::Text:: *)
(*Gives the U(1) charge(s) of an object based on a flavor symmetry *)


Charge[expr_, sym_]:= expr/. x_[__Index]:> x/. $flavorSymmetries[sym, Charges]/. 
	{\[CurlyEpsilon]-> 0, T-> 0}/. Bar@ x_-> -x;
Charge[sym_]@ expr_:= Charge[expr, sym];
