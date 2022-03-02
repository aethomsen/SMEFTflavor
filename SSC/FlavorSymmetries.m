(* ::Package:: *)

Package["SSC`"] 


(* ::Title:: *)
(*SSC`FlavorSymmetries*)


(* ::Subtitle:: *)
(*Sets up the details of the flavor symmetries  *)


(* ::Section:: *)
(*Scoping & usage definitions*)


PackageExport["$flavorSymmetries"]
PackageExport["Groups"]
PackageExport["Spurions"]
PackageExport["Charges"]
PackageExport["Representations"]
PackageExport["FieldSubstitutions"]
PackageExport["SpurionCounting"]
PackageExport["SelfConjugate"]


PackageExport["quarkOperators"]
PackageExport["leptonicOperators"]
PackageExport["semiLeptonicOperators"]


PackageExport["AddSMEFTSymmetry"]


PackageScope["$operatorClasses"]


(* ::Section:: *)
(*SMEFT operators *)


(* ::Subsection:: *)
(*Symmetries *)


(* ::Subsubsection:: *)
(*Quark symmetries*)


$quarkSymmetries= <|
	"quark:MFV"-> <|
		Groups-> <|"U3q"-> SU@ 3, "U3u"-> SU@ 3, "U3d"-> SU@ 3|>,
		Spurions-> {"Yu", "Yd"},
		Charges-> <|"q"-> {1, 0, 0}, "u"-> {0, 1, 0}, "d"-> {0, 0, 1}, "Yu"-> {1, -1, 0}, "Yd"-> {1, 0, -1}|>,
		Representations-> <|"q"-> {"U3q"@ fund}, "u"-> {"U3u"@ fund}, "d"-> {"U3d"@ fund},
			"Yu"-> {"U3q"@ fund, Bar@ "U3u"@ fund}, "Yd"-> {"U3q"@ fund, Bar@ "U3d"@ fund}|>,
		FieldSubstitutions-> <|"q"-> {"q"}, "u"-> {"u"}, "d"-> {"d"}|>,
		SpurionCounting-> <|"Yu"-> 1, "Yd"-> 2|>
	|>,
	"quark:2U2xU3"-> <|
		Groups-> <|"U2q"-> SU@ 2, "U2u"-> SU@ 2, "U3d"-> SU@ 3|>,
		Spurions-> {"Vq", "\[CapitalDelta]u", "yd", "Vb"},
		Charges-> <|"q12"-> {1, 0, 0}, "u12"-> {0, 1, 0}, "d"-> {0, 0, 1}, "q3"-> 0, "u3"-> 0,
			"Vq"-> {1, 0, 0}, "\[CapitalDelta]u"-> {1, -1, 0}, "yd"-> {1, 0, -1}, "Vb"-> {0, 0, 1}|>,
		Representations-> <|"q12"-> {"U2q"@ fund}, "u12"-> {"U2u"@ fund}, "d"-> {"U3d"@ fund},
			"Vq"-> {"U2q"@ fund}, "\[CapitalDelta]u"-> {"U2q"@ fund, Bar@ "U2u"@ fund}, 
			"yd"-> {"U2q"@ fund, Bar@ "U3d"@ fund}, "Vb"->{"U3d"@ fund}|>,
		FieldSubstitutions-> <|"q"-> {"q12", "q3"}, "u"->{"u12", "u3"}, "d"-> {"d"}|>,
		SpurionCounting-> <|"Vq"-> 1, "Vb"-> 1, "\[CapitalDelta]u"-> 2, "yd"-> 2|>
	|>,
	"quark:3U2xU1"-> <|
		Groups-> <|"U2q"-> SU@ 2, "U2u"-> SU@ 2, "U2d"-> SU@ 2|>,
		Spurions-> {"Vq", "\[CapitalDelta]u", "\[CapitalDelta]d", "Xb"},
		Charges-> <|"q12"-> {1, 0, 0, 0}, "u12"-> {0, 1, 0, 0}, "d12"-> {0, 0, 1, 0}, "q3"-> 0, "u3"-> 0, 
			"d3"-> {0, 0, 0, 1}, "Vq"-> {1, 0, 0, 0}, "\[CapitalDelta]u"-> {1, -1, 0, 0}, "\[CapitalDelta]d"-> {1, 0, -1, 0},
			"Xb"-> {0, 0, 0, -1}|>,
		Representations-> <|"q12"-> {"U2q"@ fund}, "u12"-> {"U2u"@ fund}, "d12"-> {"U2d"@ fund},
			"Vq"-> {"U2q"@ fund}, "\[CapitalDelta]u"-> {"U2q"@ fund, Bar@ "U2u"@ fund}, "\[CapitalDelta]d"-> {"U2q"@ fund, Bar@ "U2d"@ fund}|>,
		FieldSubstitutions-> <|"q"-> {"q12", "q3"}, "u"-> {"u12", "u3"}, "d"->{"d12", "d3"}|>,
		SpurionCounting-> <|"Vq"-> 1, "Xb"-> 1, "\[CapitalDelta]u"-> 2, "\[CapitalDelta]d"-> 2|>
	|>,
	"quark:3U2"-> <|
		Groups-> <|"U2q"-> SU@ 2, "U2u"-> SU@ 2, "U2d"-> SU@ 2|>,
		Spurions-> {"Vq", "\[CapitalDelta]u", "\[CapitalDelta]d"},
		Charges-> <|"q12"-> {1, 0, 0}, "u12"-> {0, 1, 0}, "d12"-> {0, 0, 1}, "q3"-> 0, "u3"-> 0, "d3"-> 0, 
			"Vq"-> {1, 0, 0}, "\[CapitalDelta]u"-> {1, -1, 0}, "\[CapitalDelta]d"-> {1, 0, -1}|>,
		Representations-> <|"q12"-> {"U2q"@ fund}, "u12"-> {"U2u"@ fund}, "d12"-> {"U2d"@ fund},
			"Vq"-> {"U2q"@ fund}, "\[CapitalDelta]u"-> {"U2q"@ fund, Bar@ "U2u"@ fund}, "\[CapitalDelta]d"-> {"U2q"@ fund, Bar@ "U2d"@ fund}|>,
		FieldSubstitutions-> <|"q"-> {"q12", "q3"}, "u"-> {"u12", "u3"}, "d"->{"d12", "d3"}|>,
		SpurionCounting-> <|"Vq"-> 1, "\[CapitalDelta]u"-> 2, "\[CapitalDelta]d"-> 2|>
	|>,
	"quark:3SU2"-> <|
		Groups-> <|"SU2q"-> SU@ 2, "SU2u"-> SU@ 2, "SU2d"-> SU@ 2|>,
		Spurions-> {"Vq", "\[CapitalDelta]u", "\[CapitalDelta]d"},
		Charges-> <|"q12"-> 0, "u12"-> 0, "d12"-> 0, "q3"-> 0, "u3"-> 0, "d3"-> 0, 
			"Vq"-> 0, "\[CapitalDelta]u"-> 0, "\[CapitalDelta]d"-> 0|>,
		Representations-> <|"q12"-> {"SU2q"@ fund}, "u12"-> {"SU2u"@ fund}, "d12"-> {"SU2d"@ fund},
			"Vq"-> {"SU2q"@ fund}, "\[CapitalDelta]u"-> {"SU2q"@ fund, Bar@ "SU2u"@ fund}, "\[CapitalDelta]d"-> {"SU2q"@ fund, Bar@ "SU2d"@ fund}|>,
		FieldSubstitutions-> <|"q"-> {"q12", "q3"}, "u"-> {"u12", "u3"}, "d"->{"d12", "d3"}|>,
		SpurionCounting-> <|"Vq"-> 1, "\[CapitalDelta]u"-> 2, "\[CapitalDelta]d"-> 2|>
	|>,
	"quark:none"-><|
		Groups-> <||>,
		Spurions-> {},
		Charges-> <|"q1"-> 0, "q2"-> 0, "q3"-> 0, "u1"-> 0, "u2"-> 0, "u3"-> 0, "d1"-> 0, "d2"-> 0, "d3"-> 0|>,
		Representations-> <||>,
		FieldSubstitutions-> <|"q"-> {"q1", "q2", "q3"}, "u"-> {"u1", "u2", "u3"}, "d"-> {"d1", "d2", "d3"}|>,
		SpurionCounting-> <||>
	|>
|>;


(* ::Subsubsection:: *)
(*Lepton symmetries*)


$leptonSymmetries= <|
	"lep:MFV"-> <|
		Groups-> <|"U3l"-> SU@ 3, "U3e"-> SU@ 3|>,
		Spurions-> {"Ye"},
		Charges-> <|"l"-> {1, 0}, "e"-> {0, 1}, "Ye"->{1, -1}|>,
		Representations-> <|"l"->{"U3l"@ fund}, "e"-> {"U3e"@ fund}, "Ye"-> {"U3l"@ fund, Bar@ "U3e"@ fund}|>,
		FieldSubstitutions-> <|"l"-> {"l"}, "e"-> {"e"}|>,
		SpurionCounting-> <|"Ye"-> 2|>
	|>,
	"lep:2U2x2U1"-> <|
		Groups-> <|"U2l"-> SU@ 2, "U2e"-> SU@ 2|>,
		Spurions-> {"Vl", "Ve", "\[CapitalDelta]e", "X\[Tau]"},
		Charges-> <|"l12"-> {1, 0, 0, 0}, "l3"-> {0, 0, 1, 0}, "e12"->{0, 1, 0, 0}, "e3"-> {0, 0, 0, 1}, 
			"Vl"-> {1, 0, 0, -1}, "Ve"-> {0, 1, -1, 0}, "\[CapitalDelta]e"-> {1, -1, 0, 0}, "X\[Tau]"-> {0, 0, 1, -1}|>,
		Representations-> <|"l12"-> {"U2l"@ fund}, "e12"-> {"U2e"@ fund}, "Vl"-> {"U2l"@ fund}, "Ve"-> {"U2e"@ fund}, 
			"\[CapitalDelta]e"-> {"U2l"@ fund, Bar@ "U2e"@ fund}|>,
		FieldSubstitutions-> <|"l"-> {"l12", "l3"}, "e"-> {"e12", "e3"}|>,
		SpurionCounting-> <|"Vl"-> 1, "Ve"-> 1, "\[CapitalDelta]e"-> 2, "X\[Tau]"-> 1|>
	|>,
	"lep:2U2xU1"-> <|
		Groups-> <|"U2l"-> SU@ 2, "U2e"-> SU@ 2|>,
		Spurions-> {"Vl", "Ve", "\[CapitalDelta]e", "X\[Tau]"},
		Charges-> <|"l12"-> {1, 0, 0}, "l3"-> {0, 0, 0}, "e12"->{0, 1, 0}, "e3"-> {0, 0, 1}, 
			"Vl"-> {1, 0, -1}, "\[CapitalDelta]e"-> {1, -1, 0}, "X\[Tau]"-> {0, 0, -1}|>,
		Representations-> <|"l12"-> {"U2l"@ fund}, "e12"-> {"U2e"@ fund}, "Vl"-> {"U2l"@ fund},  
			"\[CapitalDelta]e"-> {"U2l"@ fund, Bar@ "U2e"@ fund}|>,
		FieldSubstitutions-> <|"l"-> {"l12", "l3"}, "e"-> {"e12", "e3"}|>,
		SpurionCounting-> <|"Vl"-> 1, "\[CapitalDelta]e"-> 2, "X\[Tau]"-> 1|>
	|>,
	"lep:2U2"-> <|
		Groups-> <|"U2l"-> SU@ 2, "U2e"-> SU@ 2|>,
		Spurions-> {"Vl", "\[CapitalDelta]e"},
		Charges-> <|"l12"-> {1, 0}, "l3"-> 0, "e12"-> {0, 1}, "e3"-> 0, "Vl"-> {1, 0}, "\[CapitalDelta]e"->{1, -1}|>,
		Representations-> <|"l12"-> {"U2l"@ fund}, "e12"-> {"U2e"@ fund}, "Vl"-> {"U2l"@ fund}, 
			"\[CapitalDelta]e"-> {"U2l"@ fund, Bar@ "U2e"@ fund}|>,
		FieldSubstitutions-> <|"l"-> {"l12", "l3"}, "e"-> {"e12", "e3"}|>,
		SpurionCounting-> <|"Vl"-> 1, "\[CapitalDelta]e"-> 2|>
	|>,
	"lep:U3diag"-> <|
		Groups-> <|"U3l"-> SU@ 3|>,
		Spurions-> {"\[CapitalDelta]l"},
		Charges-> <|"l"-> 0, "e"-> 0, "\[CapitalDelta]l"-> 0|>,
		Representations-> <|"l"-> {"U3l"@ fund}, "e"-> {"U3l"@ fund}, "\[CapitalDelta]l"-> {"U3l"@ adj}|>,
		FieldSubstitutions-> <|"l"-> {"l"}, "e"-> {"e"}|>,
		SpurionCounting-> <|"\[CapitalDelta]l"-> 2|>,
		SelfConjugate-> {"\[CapitalDelta]l"}
	|>,
	"lep:2SU2"-> <|
		Groups-> <|"U2l"-> SU@ 2, "U2e"-> SU@ 2|>,
		Spurions-> {"Vl", "\[CapitalDelta]e"},
		Charges-> <|"l12"-> 0, "l3"-> 0, "e12"-> 0, "e3"-> 0, "Vl"-> 0, "\[CapitalDelta]e"-> 0|>,
		Representations-> <|"l12"-> {"U2l"@ fund}, "e12"-> {"U2e"@ fund}, "Vl"-> {"U2l"@ fund},
			"\[CapitalDelta]e"-> {"U2l"@ fund, Bar@ "U2e"@ fund}|>,
		FieldSubstitutions-> <|"l"-> {"l12", "l3"}, "e"-> {"e12", "e3"}|>,
		SpurionCounting-> <|"Vl"-> 1, "\[CapitalDelta]e"-> 2|>
	|>,
	"lep:U2diag"-> <|
		Groups-> <|"U2l"-> SU@ 2|>,
		Spurions-> {"\[CapitalDelta]l"},
		Charges-> <|"l12"-> {1}, "l3"-> 0, "e12"-> {1}, "e3"-> 0, "\[CapitalDelta]l"-> 0, "Vl"-> {1}|>,
		Representations-> <|"l12"-> {"U2l"@ fund}, "e12"-> {"U2l"@ fund}, 
			"\[CapitalDelta]l"-> {"U2l"@ adj}|>,
		FieldSubstitutions-> <|"l"-> {"l12", "l3"}, "e"-> {"e12", "e3"}|>,
		SpurionCounting-> <|"\[CapitalDelta]l"-> 2|>,
		SelfConjugate-> {"\[CapitalDelta]l"}
	|>,
	"lep:6U1"-> <|
		Groups-> <||>,
		Spurions-> {"Ye1", "Ye2", "Ye3"},
		Charges-> <|"l1"-> {1, 0, 0, 0, 0, 0}, "l2"-> {0, 0, 1, 0, 0, 0}, "l3"-> {0, 0, 0, 0, 1, 0}, 
			"e1"-> {0, 1, 0, 0, 0, 0}, "e2"-> {0, 0, 0, 1, 0, 0}, "e3"-> {0, 0, 0, 0, 0, 1}, 
			"Ye1"-> {1, -1, 0, 0, 0, 0}, "Ye2"-> {0, 0, 1, -1, 0, 0}, "Ye3"-> {0, 0, 0, 0, 1, -1}|>,
		Representations-> <||>,
		FieldSubstitutions-> <|"l"-> {"l1", "l2", "l3"}, "e"-> {"e1", "e2", "e3"}|>,
		SpurionCounting-> <|"Ye1"-> 3, "Ye2"-> 2, "Ye3"-> 1|>
	|>,
	"lep:3U1A"-> <|
		Groups-> <||>,
		Spurions-> {"Ye1", "Ye2", "Ye3"},
		Charges-> <|"l1"-> {1, 0, 0}, "l2"-> {0, 1, 0}, "l3"-> {0, 0, 1}, 
			"e1"-> {-1, 0, 0}, "e2"-> {0, -1, 0}, "e3"-> {0, 0, -1},
			"Ye1"-> {2, 0, 0}, "Ye2"-> {0, 2, 0}, "Ye3"-> {0, 0, 2}|>,
		Representations-> <||>,
		FieldSubstitutions-> <|"l"-> {"l1", "l2", "l3"}, "e"-> {"e1", "e2", "e3"}|>,
		SpurionCounting-> <|"Ye1"-> 3, "Ye2"-> 2, "Ye3"-> 1|>
	|>,
		"lep:3U1V"-> <|
		Groups-> <||>,
		Spurions-> {"Ye1", "Ye2", "Ye3"},
		Charges-> <|"l1"-> {1, 0, 0}, "l2"-> {0, 1, 0}, "l3"-> {0, 0, 1}, 
			"e1"-> {1, 0, 0}, "e2"-> {0, 1, 0}, "e3"-> {0, 0, 1}|>,
		Representations-> <||>,
		FieldSubstitutions-> <|"l"-> {"l1", "l2", "l3"}, "e"-> {"e1", "e2", "e3"}|>,
		SpurionCounting-> <||>
	|>,
	"lep:none"-> <|
		Groups-> <||>,
		Spurions-> {},
		Charges-> <|"l1"-> 0, "l2"-> 0, "l3"-> 0, "e1"-> 0, "e2"-> 0, "e3"-> 0|>,
		Representations-> <||>,
		FieldSubstitutions-> <|"l"-> {"l1", "l2", "l3"}, "e"-> {"e1", "e2", "e3"}|>,
		SpurionCounting-> <||>
	|>
|>;


(* ::Subsubsection:: *)
(*Mixed symmetries*)


$mixedSymmetries= <||>;


(* ::Subsection:: *)
(*Product symmetries*)


MakeProductSymmetries[]:= Module[{quark, lep},
	Association@ Table[
			{quark, lep}-> JoinQuarkLeptonSymmetry[quark, lep]
		, {quark, Keys@ $quarkSymmetries}, {lep, Keys@ $leptonSymmetries}]
];


JoinQuarkLeptonSymmetry[quarkSym_, lepSym_]:= Module[{atr, lU1s, qU1s, sym=<||>, 
		qSym= $quarkSymmetries@ quarkSym, lSym= $leptonSymmetries@ lepSym},
	Do[sym@ atr= Join[qSym@ atr, lSym@ atr];
		, {atr, {Groups, Spurions, Representations, FieldSubstitutions, SpurionCounting}}];
	qU1s= If[MatchQ[List@@ qSym@ Charges, {0..}], 0, Length@ FirstCase[qSym@ Charges, _List] ];
	lU1s= If[MatchQ[List@@ lSym@ Charges, {0..}], 0, Length@ FirstCase[lSym@ Charges, _List] ];
	sym@ Charges= Join[
		If[# === 0, 0, PadRight[#, qU1s+ lU1s]]&/@ qSym@ Charges,
		If[# === 0, 0, PadLeft[#, qU1s+ lU1s]]&/@ lSym@ Charges
	];
	sym@ SelfConjugate= Lookup[qSym, SelfConjugate, {}]~ Join~ Lookup[lSym, SelfConjugate, {}];
	sym
];


(* ::Text:: *)
(*Collect flavor symmetries *)


UpdateSymmetries[]:=(
	$flavorSymmetries= Join[$leptonSymmetries, 
		$quarkSymmetries, 
		MakeProductSymmetries[],
		$mixedSymmetries];);
UpdateSymmetries[];


(* ::Subsection:: *)
(*Add Symmetry*)


AddSMEFTSymmetry::unkwntype= "Enter a valid symmetry type (\"Lepton\", \"Quark\", \"Mixed\")"
AddSMEFTSymmetry[symType_, name_String-> groupInfo_Association]:= Block[{},
	Switch[symType
	,"Lepton",
		$leptonSymmetries@ name= groupInfo;
	,"Quark",
		$quarkSymmetries@ name= groupInfo;
	,"Mixed",
		$mixedSymmetries@ name= groupInfo;
	,_,
		Message[AddSMEFTSymmetry::unkwntype];
		Abort[];	
	];
	UpdateSymmetries[];
]
