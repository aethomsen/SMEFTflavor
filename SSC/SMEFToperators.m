(* ::Package:: *)

Package["SSC`"] 


(* ::Title:: *)
(*SSC`SMEFToperators*)


(* ::Subtitle:: *)
(*Defines the fermionic SMEFT operators *)


(* ::Section:: *)
(*Scoping & usage definitions*)


PackageExport["$smeftOperators"]
PackageExport["Fields"]
PackageExport["Multiplicity"]
PackageExport["PermutationSymmetries"]
PackageExport["UniqueHc"]


PackageExport["quarkOperators"]
PackageExport["leptonicOperators"]
PackageExport["semiLeptonicOperators"]


PackageScope["$operatorClasses"]


(* ::Section:: *)
(*SMEFT operators *)


(* ::Subsection:: *)
(*Operator properties *)


$smeftOperators= <|
(*Quark operators *)
	"OuH"-> <|
			Fields-> {Bar@q, u},
			UniqueHc-> True
		|>,
	"OdH"-> <|
			Fields-> {Bar@q, d},
			UniqueHc-> True
		|>,
	"Ou(B,W,G)"-> <|
			Fields-> {Bar@ q, u},
			Multiplicity-> 3,
			UniqueHc-> True
		|>,
	"Od(B,W,G)"-> <|
			Fields-> {Bar@ q, d},
			Multiplicity-> 3,
			UniqueHc-> True
		|>,
	"OHq(1,3)"-> <|
			Fields-> {Bar@ q, q},
			Multiplicity-> 2,
			UniqueHc-> False
		|>,
	"OHu"-> <|
			Fields-> {Bar@ u, u},
			Multiplicity-> 1,
			UniqueHc-> False
		|>,
	"OHd"-><|
			Fields-> {Bar@ d, d},
			Multiplicity-> 1,
			UniqueHc-> False
		|>,
	"OHud"-> <|
			Fields-> {Bar@ u, d},
			Multiplicity-> 1,
			UniqueHc-> True
		|>,
	"Oqq(1,3)"-> <|
			Fields-> {Bar@ q, q, Bar@ q, q},
			PermutationSymmetries-> {{3, 4, 1, 2}},
			Multiplicity-> 2,
			UniqueHc-> False
		|>,
	"Ouu"-> <|
			Fields-> {Bar@ u, u, Bar@u, u},
			PermutationSymmetries-> {{3, 4, 1, 2}},
			UniqueHc-> False
		|>,
	"Odd"-> <|
			Fields-> {Bar@ d, d, Bar@ d, d},
			PermutationSymmetries-> {{3, 4, 1, 2}},
			UniqueHc-> False
		|>,
	"Oud(1,8)"-> <|
			Fields-> {Bar@ u, u, Bar@ d, d},
			UniqueHc-> False,
			Multiplicity-> 2
		|>,
	"Oqu(1,8)"-> <|
			Fields->{Bar@ q, q, Bar@ u, u},
			UniqueHc-> False,
			Multiplicity-> 2
		|>,
	"Oqd(1,8)"-> <|
			Fields-> {Bar@ q, q, Bar@ d, d},
			UniqueHc-> False,
			Multiplicity-> 2
		|>,
	"Oquqd(1,8)"-> <|
			Fields-> {Bar@ q, u, Bar@ q, d},
			UniqueHc-> True,
			Multiplicity-> 2
		|>,
(*Leptonic operators *)
	"OeH"-> <|
			Fields-> {Bar@ l, e},
			UniqueHc-> True
		|>,
	"Oe(B,W)"-> <|
			Fields-> {Bar@ l, e},
			Multiplicity-> 2,
			UniqueHc-> True
		|>,
	"OHl(1,3)"-> <|
			Fields-> {Bar@ l, l},
			Multiplicity-> 2,
			UniqueHc-> False
		|>,
	"OHe"-> <|
			Fields-> {Bar@ e, e},
			UniqueHc-> False
		|>,
	"Oll"-> <|
			Fields-> {Bar@ l, l, Bar@ l, l},
			PermutationSymmetries-> {{3, 4, 1 ,2}},
			UniqueHc-> False
		|>,
	"Oee"-> <|
			Fields-> {Bar@ e, e, Bar@ e, e},
			PermutationSymmetries-> {{1, 4, 3, 2}, {3, 4, 1, 2}},
			UniqueHc-> False
		|>,
	"Ole"-><|
			Fields-> {Bar@ l, l, Bar@ e, e},
			UniqueHc-> False
		|>,
(*Quark-lepton operators*)
	"Olq(1,3)"-> <|
			Fields-> {Bar@ l, l, Bar@ q, q},
			UniqueHc-> False,
			Multiplicity-> 2
		|>,
	"Oeu"-> <|
			Fields-> {Bar@ e, e, Bar@ u, u},
			UniqueHc-> False
		|>,
	"Oed"-> <|
			Fields-> {Bar@ e, e, Bar@ d, d},
			UniqueHc-> False
		|>,
	"Olu"-> <|
			Fields-> {Bar@ l, l, Bar@ u, u},
			UniqueHc-> False
		|>,
	"Old"-> <|
			Fields-> {Bar@ l, l, Bar@ d, d},
			UniqueHc-> False
		|>,
	"Oqe"-> <|
			Fields-> {Bar@ q, q, Bar@ e, e},
			UniqueHc-> False
		|>,
	"Oledq"-> <|
			Fields-> {Bar@ l, e, Bar@ d, q},
			UniqueHc-> True
		|>,
	"Olequ(1,3)"-> <|
			Fields-> {Bar@ l, e, Bar@ q, u},
			UniqueHc-> True,
			Multiplicity-> 2
		|>
|>;


(* ::Subsection:: *)
(*Lists and tables *)


$operatorClasses= <|
		"\!\(\*SuperscriptBox[\(\[Psi]\), \(2\)]\)\!\(\*SuperscriptBox[\(H\), \(3\)]\)"-> {"OuH", "OdH", "OeH"},
		"\!\(\*SuperscriptBox[\(\[Psi]\), \(2\)]\)XH"-> {"Ou(B,W,G)", "Od(B,W,G)", "Oe(B,W)"},
		"\!\(\*SuperscriptBox[\(\[Psi]\), \(2\)]\)\!\(\*SuperscriptBox[\(H\), \(2\)]\)D"-> {"OHq(1,3)", "OHu", "OHd", "OHud", "OHl(1,3)", "OHe"},
		"(LL)(LL)"-> {"Oqq(1,3)", "Oll", "Olq(1,3)"},
		"(RR)(RR)"-> {"Ouu", "Odd", "Oud(1,8)", "Oee", "Oeu", "Oed"},
		"(LL)(RR)"-> {"Oqu(1,8)", "Oqd(1,8)", "Ole", "Olu", "Old", "Oqe"},
		"(LR)(LR)"-> {"Oquqd(1,8)", "Olequ(1,3)"},
		"(LR)(RL)"-> {"Oledq"}
	|>;


quarkOperators= {"OuH", "OdH", "Ou(B,W,G)", "Od(B,W,G)", "OHq(1,3)", "OHu", "OHd", "OHud", 
	"Oqq(1,3)", "Ouu", "Odd", "Oud(1,8)", "Oqu(1,8)", "Oqd(1,8)", "Oquqd(1,8)"};
leptonicOperators= {"OeH", "Oe(B,W)", "OHl(1,3)", "OHe", "Oll", "Oee", "Ole"};
semiLeptonicOperators= {"Olq(1,3)", "Oeu", "Oed", "Olu", "Old", "Oqe", "Oledq", "Olequ(1,3)"};
