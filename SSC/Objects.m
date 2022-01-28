(* ::Package:: *)

Package["SSC`"] 


(* ::Title:: *)
(*SSC`Objects*)


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


PackageScope["ComplexRepQ"]


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


(* ::Subsection:: *)
(*Spur *)


SetAttributes[Spur, Orderless];
Bar@ x_Spur:= Bar/@ x;


(* ::Subsection:: *)
(*CGs*)


SetAttributes[CGs, Orderless];
Bar@ x_CGs:= Bar/@ x;
