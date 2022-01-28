(* ::Package:: *)

Package["SSC`"] 


(* ::Title:: *)
(*SSC`Operators*)


(* ::Subtitle:: *)
(*Functionality for finding basis operators for an operator*)


(* ::Section:: *)
(*Scoping & usage definitions*)


PackageExport["ConstructSinglets"]


(* ::Section:: *)
(*Determine operator basis *)


(* ::Subsection:: *)
(*Determine singlet operators  *)


(* ::Text:: *)
(*Find all singlets contraction of a SMEFT operator given a set of spurions and a flavor symmetry*)


ConstructSinglets[SMEFTop_, flavorSym_, spurions_Spur]:= Module[{contractions, spurCharge, operators},
	operators= OperatorSplit[SMEFTop, flavorSym];
	spurCharge= Plus@@ Charge[spurions, flavorSym];
	(*Throw away operators with nonzero charge*)
	operators= DeleteCases[operators, _?(!MatchQ[Plus@@ Charge[#, flavorSym]+ spurCharge, {0..}| 0]&)];
	contractions= Reap[Do[
		op= Operator[op, spurions, CGs[]];
		op= DetermineContractions[op, flavorSym];
		Sow@ op;
	,{op, operators}]][[2]];
	If[Length@ contractions === 0, Return@ {}];
	Flatten[First@ contractions, 1]
]


(* ::Subsubsection:: *)
(*Field insertions*)


(* ::Text:: *)
(*Populates a SMEFT operator with all possible field decomposition*)


OperatorSplit::unkwnOp="`1` does not match any SMEFT operators.";
OperatorSplit[SMEFTop_, flavorSym_]:= Module[{field, fieldSubs, fields},
	fields= Lookup[$smeftOperators, SMEFTop,
		Message[OperatorSplit::unkwnOp, SMEFTop]; Abort[];]@ Fields;
	fieldSubs= $flavorSymmetries[flavorSym, FieldSubstitutions];
	If[!SubsetQ[Keys@ fieldSubs, fields/. Bar-> Identity], Return@ {};];
	fields/. fieldSubs// Tuples
]


(* ::Subsubsection:: *)
(*Index contractions *)


(* ::Text:: *)
(*Find all contractions of the fields in an operator*)


dummyInds= Table[Symbol["$d"<> ToString@ n], {n, 100}];


DetermineContractions[op_Operator, flavorSym_]:= Module[{inds, addedCGs, contractions, 
		newOp, opSeed, objSubs, n=1, cg, representations, groups, gr, indSet},
	representations= Lookup[$flavorSymmetries@ flavorSym, Representations, <||>];
	groups=Lookup[$flavorSymmetries@ flavorSym, Groups, <||>];
	
	(*Eliminate non-singlets*)
	inds= List@@@ List@@ op[[;;2]]/. representations// Flatten;
	inds= DeleteCases[inds, _?(FreeQ[Alternatives@@ Keys@ groups])];
	inds= GroupBy[inds, Level[#, {-2}, Head]&];
	Catch@ Do[
		Switch[groups@ gr
		,SU@ 2,
			indSet= inds@ gr/. gr-> Identity/. Bar-> Identity;
			If[indSet === {adj}, Throw@ Return[{}];];
			If[OddQ@ Count[indSet, fund], Throw@ Return[{}];];
		,SU@ 3,
			indSet= inds@ gr/. gr-> Identity;
			If[indSet === {adj}, Throw@ Return[{}]];
			If[Count[indSet, fund] =!= Count[indSet, Bar@ fund], Throw@ Return[{}]];
		]
	,{gr, Keys@ inds}];
	
	(*Insert uncontracted indices for each flavored object*)
	objSubs= representations;
	objSubs= (Bar/@ KeyMap[Bar]@ objSubs)~ Join~ objSubs;
	objSubs= KeyValueMap[RuleDelayed[#1, #1@@ (Index[#, dummyInds[[n++]]]&)/@ #2]&]@ objSubs;
	opSeed= op/. objSubs;
	
	(*Loop over various added CGs *)
	addedCGs={CGs[]};
	(*TODO: Find all reasonable cgs to insert*)
	Flatten[Table[
		newOp= opSeed; newOp[[-1]]= Join[newOp[[-1]], cg];
		inds= Cases[newOp, _Index, All];
		contractions= newOp/. IndexContractions[inds, flavorSym];
		
		(*Add epsilons in SU(2) contraction between non-conjugate indices*)
		contractions= AddSU2Epsilons[#, flavorSym]&/@ contractions
		
	,{cg, addedCGs}], 1]	
]


(* ::Text:: *)
(*Find all valid contractions in a list of indices *)


IndexContractions[inds_, flavorSym_]:=Module[{indTypes, type, inds1, inds2},
	indTypes= DeleteDuplicates[inds[[;;,1]]/. Bar-> Identity];
	(*Combine all permutation for each index type*)
	Join@@@ Tuples@ Table[
		(*Account for whether the index type is complex or not*)
		Apply[Rule, If[ComplexRepQ[type, flavorSym],
			inds1= Cases[inds, Index[Bar@ type, _]][[;;,2]];
			inds2= Cases[inds, Index[type, _]][[;;,2]];
			If[Length@ inds1 =!= Length@ inds2, Return@ {}];
			Transpose/@ Thread[{inds1, Permutations@ inds2}, List, {2}]
		,
			inds1= Cases[inds, Index[type| Bar@ type, _]][[;;, 2]];
			If[OddQ@ Length@ inds1, Return@ {}];
			Pairs@ inds1
		], {2}]
	,{type, indTypes}]
]
Pairs@ set_:= DeleteDuplicatesBy[Partition[#, 2]&/@ Permutations@ set, (Sort[Sort/@ #] &)];


(* ::Text:: *)
(*Insert SU(2) epsilons where needed *)


AddSU2Epsilons[op_Operator, flavorSym_]:= Module[{ind, inds, su2Groups, cgs, pos, unique, opOut=op},
	inds= Cases[Cases[op, _Index, All],
		Index[(Bar@ gr_@ fund| (gr:Except@ Bar)@ fund), _]/; 
			$flavorSymmetries[flavorSym, Groups, gr] === SU@ 2];
	inds= DeleteCases[Tally@ inds, {_, 1}];
	If[Length@ inds===0, Return@ op];
	inds= inds[[;;, 1]];
	cgs= CGs@@ Table[
		unique= MapAt[Unique, ind, 2];
		opOut= ReplacePart[FirstPosition[opOut, ind]-> unique]@ opOut;
		\[CurlyEpsilon][unique, ind]/. x_Index-> Bar@ x
	, {ind, inds}];
	opOut[[-1]]= Join[opOut[[-1]], cgs];
	opOut
]

