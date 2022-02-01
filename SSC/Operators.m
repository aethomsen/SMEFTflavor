(* ::Package:: *)

Package["SSC`"] 


(* ::Title:: *)
(*SSC`Operators*)


(* ::Subtitle:: *)
(*Functionality for finding basis operators for an operator*)


(* ::Section:: *)
(*Scoping & usage definitions*)


PackageExport["BasisCount"]
PackageExport["ConstructSinglets"]
PackageExport["CountingTable"]
PackageExport["OperatorBasis"]
PackageExport["OperatorSpurionBasis"]


PackageExport["SMEFToperators"]
PackageExport["SpurionCount"]


(* ::Section:: *)
(*Determine operator basis *)


(* ::Subsection::Closed:: *)
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


(* ::Subsubsection::Closed:: *)
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


(* ::Subsubsection::Closed:: *)
(*Index contractions *)


(* ::Text:: *)
(*Find all contractions of the fields in an operator*)


dummyInds= Table[Symbol["$d"<> ToString@ n], {n, 100}];


DetermineContractions::adjs= "The current implementation supports no more than two adjoint-indexed spurions of any one group.";


DetermineContractions[op_Operator, flavorSym_]:= Module[{inds, addedCGs, cgChoices, contractions, 
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
	cgChoices= Table[
		indSet= inds@ gr/. gr-> Identity;
		Switch[Count[indSet, adj]
		,0,
			Nothing
		,1,
			{{T[Index[gr@ adj, dummyInds[[n++]]], Index[gr@ fund, dummyInds[[n++]]], 
				Index[Bar@ gr@ fund, dummyInds[[n++]]]]}} 
		,2,
			{{T[Index[gr@ adj, dummyInds[[n++]]], Index[gr@ fund, dummyInds[[n++]]], 
				Index[Bar@ gr@ fund, dummyInds[[n++]]]],
			T[Index[gr@ adj, dummyInds[[n++]]], Index[gr@ fund, dummyInds[[n++]]], 
				Index[Bar@ gr@ fund, dummyInds[[n++]]]]}}
		,_,
			Message[DetermineContractions::adjs];
			Abort[];
		]
	, {gr, Keys@ inds}];
	addedCGs= CGs@@@ Flatten/@ Tuples@ cgChoices; 
	(*addedCGs= {CGs[]}; *)
	
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


(* ::Subsection:: *)
(*Operator actions and checks*)


(* ::Subsubsection::Closed:: *)
(*Checks redundancy *)


(* ::Text:: *)
(*Checks if part of the spurions form a singlet, which can then be removed (e.g. Subscript[V, q]^a Subscript[\!\(\*OverscriptBox[\(V\), \(_\)]\), qa])*)


ReducibleOperatorQ[op_Operator, flavorSym_]:= Module[{objects, opens, pos, contractedGroups, group},
	objects= Flatten[List@@@ DeleteCases[List@@ op[[2;;]], 1]];
	(*Discount all objects which contract to external indices. These can never be reduced*)
	While[Length[opens= DeleteCases[
			Tally@ Cases[objects, (ind_Index:> (ind/. Bar-> Identity)), All], {_, 2}][[;;, 1]]]> 0,
		pos= Position[objects, Alternatives@@ Join[opens, Bar@ opens]][[;;, {1}]];
		objects= Delete[objects, pos];
	];
	If[Length@ objects === 0, Return@ False];
	
	(*Determine singlet contractions of non-Abelian indices*)
	contractedGroups= Position[objects, #| Bar@ #][[;;, 1]]&/@
		DeleteDuplicates@ Cases[objects, (ind_Index:> (ind/. Bar-> Identity)), All];
	contractedGroups= DeleteDuplicates/@ (contractedGroups//. 
		{OrderlessPatternSequence[{a___, x_, b___}, {c___, x_, d___}, rest___]}->
			{{x, a, b, c, d}, rest});
	contractedGroups= Join[contractedGroups, List/@ Complement[Range@ Length@ objects,
		Flatten@ contractedGroups]];
		
	(*Determine of any combination of contracted groups have charge 0,
	in which case they can be removed*)
	objects =Table[objects[[group]], {group, contractedGroups}];
	objects=Plus@@@ Charge[flavorSym]@ objects;
	If[FreeQ[Plus@@@ Subsets[objects, {1, Infinity}], {0..}| 0, 1], Return@ False;];

	True
]
ReducibleOperatorQ[flavorSym_]@ op_:= ReducibleOperatorQ[op, flavorSym];


(* ::Subsubsection::Closed:: *)
(*Conjugation*)


(* ::Text:: *)
(*Function to determine Hermitian conjugate of an operator*)


ConjugateOperator::invldFields= "Operator has `1` fields.";
ConjugateOperator[Operator[fields_, spurs_Spur, cgs_]]:= Module[{cFields, cSpurs, cCGs},
	cFields= Bar/@ Switch[Length@ fields, 2, fields[[{2, 1}]], 4, fields[[{2, 1, 4, 3}]], _,
		Message[ConjugateOperator::invldFields, Length@ fields]; Abort[];];
	cSpurs= Bar/@ spurs;
	(*Conjugate CGs*)
	cCGs= cgs/. {e_\[CurlyEpsilon]-> -e, T[A_, a_, b_]:> T[A, b, a]}//. CGs[n_Integer cg_, x___]:>n CGs[cg, x];
	
	Operator[cFields, cSpurs, cCGs]/. Index[rep_, lab_]:> Index[Bar@ rep, lab]/.
		Operator[f_, s_, n_Integer c_CGs]:> n Operator[f, s, c]
]


(* ::Text:: *)
(*Check if a given operator is Hermitian *)


SelfConjugateQ[op_Operator, SMEFTop_]:= Module[{pat= OperatorPattern@ op, newOp, opSyms, sym, out= False},
	opSyms= Lookup[$smeftOperators@ SMEFTop, PermutationSymmetries, {}];
	AppendTo[opSyms, Range@ Length@ First@ op];
	Do[
		newOp= op; newOp[[1]]= newOp[[1, sym]]; 
		If[MatchQ[ConjugateOperator@ newOp, pat], out= True; Return[]; ];
	,{sym, opSyms}];
	out
];
SelfConjugateQ[SMEFTop_]@ op_:= SelfConjugateQ[op, SMEFTop];


(* ::Subsection:: *)
(*Determine operator identities *)


(* ::Subsubsection:: *)
(*Operator Patterns *)


(* ::Text:: *)
(*Set up a pattern to identify an Operator object with its OpID object*)


patternLabels= Table[Unique@ ordPat, 20];
OperatorIdentificationPattern[id_, op_Operator]:= Module[{antisyms, pattern, sign, counter= 1},
	pattern= OperatorPattern@ op;
	(*Determine the sign from canonical ordering of \[CurlyEpsilon] arguments*)
	sign= Power[-1, Count[Last@ op, _\[CurlyEpsilon]? (Not@* OrderedQ), All]];
	
	(*Name the antisymmetrized pattern indices*)
	pattern= pattern/. {obj: (_\[CurlyEpsilon]):> 
		(obj/. x_OrderlessPatternSequence:> Pattern[Evaluate@ patternLabels[[counter++]], x])};
	(*Provide sign signatures for all the anti-symmetric indices*)
	antisyms= Times@@ Cases[pattern, 
			Verbatim[Pattern][name_, pats_OrderlessPatternSequence]:> 
				Inactive[Signature]@ {name} Inactive[Signature][List@@ pats/. Verbatim[Pattern][x_, Blank[]]:>x]
		, All];
	(*Produce substitution rule from operator pattern*)
	With[{temp= antisyms, s=sign},
		RuleDelayed[pattern, s  Activate@ temp OpID[id]]
	]
];


(* ::Text:: *)
(*Create a pattern from an operator *)


OperatorPattern::openinds= "`1` contains open indices.";
OperatorPattern[op_Operator]:= Module[{indices},
	indices=Tally@ Cases[op, ind:Index[__]:> (ind/. Bar-> Identity), All];
	If[Length@ Cases[indices, {_, 1}][[;;, 1]]> 0, 
		Message[OperatorPattern::openinds, Echo@ op];
		Abort[];
	];
	indices= IndexPatternReplace/@ indices[[;;, 1]];
	op/. indices/. specialSubs
]


IndexPatternReplace@ Index[rep_, lab_]:= With[{temp= lab},
	Rule[Index[x:(rep| Bar@ rep), lab], Index[x, Pattern[temp, Blank[]]]]
];


specialSubs= {
		\[CurlyEpsilon][\[Mu]__]:> \[CurlyEpsilon]@ OrderlessPatternSequence@ \[Mu]
	};


(* ::Text:: *)
(*Set up a pattern to expand an  OpID object to the corresponding operator *)


OperatorExpansionPattern[id_, op_Operator]:= Module[{},
	Rule[OpID@ id, op]
];


(* ::Subsubsection::Closed:: *)
(*Match operators to pattern*)


(* ::Text:: *)
(*Global associations with operator pattern substitutions (both ways)*)


ResetOperatorPatterns[]:= Block[{},
	$operatorPatterns= {};
	$operatorIdentifiers= {};
];
ResetOperatorPatterns[];


(* ::Text:: *)
(*Substitutes all operators in an expression with OpID objects for further manipulations  *)


MatchOperatorPatterns@ expr_:= Module[{ops},
	(*New patterns are constructed for the operators*)
	ops= DeleteDuplicates@ Cases[expr, _Operator, All];
	MakeNewOperatorPatterns@ ops;
	
	expr/. $operatorPatterns
];


(* ::Text:: *)
(*Makes new pattern rules for a list of operators of a given type*)


MakeNewOperatorPatterns[opList_List]:= Module[{identifiers, newRules, nextID, op, 
		pat, rule, rules, remainingOps},	
	rules= $operatorPatterns;
	remainingOps= DeleteCases[opList, Alternatives@@ rules[[;;, 1]]];
	If[Length@ remainingOps === 0, Return[];];
	identifiers= $operatorIdentifiers;
	
	(*Loops through the unidentitfied operators, creating new patterns for them*)
	nextID= Length@ rules+ 1; pat= Alternatives[];
	newRules= Reap[Do[
		If[MatchQ[pat]@ op, Continue[]; ];
		rule= Sow[OperatorIdentificationPattern[nextID, op], 1]; 
		AppendTo[pat, First@ rule];
		Sow[OperatorExpansionPattern[nextID++, op], 2];
	, {op, remainingOps}], {1, 2}][[2, ;;, 1]];
	
	$operatorPatterns= rules~ Join~ newRules[[1]];
	$operatorIdentifiers= identifiers~ Join~ newRules[[2]];
];


(* ::Subsubsection:: *)
(*Make identities *)


(* ::Text:: *)
(*Construct all identities based on the current content of $operatorPatterns*)


Options@ ConstructOperatorIdentities= {IncludeHc-> False};
ConstructOperatorIdentities[SMEFTop_, flavorSym_, OptionsPattern[]]:= Module[
		{eps, genCount, genInds, group, groups, index, indices, identity, identities, len, lhs, n, newOp, 
			op, opIdentities, ops, opSyms, ordering, pair, pairs, revOrdering, rhs,  sym, 
			opID= 1},
	If[(len= Length@ $operatorIdentifiers) === 0, 
		Return[];
	];
	opSyms=Lookup[$smeftOperators@ SMEFTop, PermutationSymmetries, {}];

	(*Loop to make all identities of the operator class*)
	identities= Flatten@ Reap[While[opID<= len,
		op= OpID@ opID/. $operatorIdentifiers;
		(*Fermion permutations - accoriding to SMEFT symmetries*)
		opIdentities= Table[
				newOp=op; newOp[[1]]= newOp[[1,sym]];
				op- newOp
			, {sym, opSyms}];
		
		(*SU(2)\[CurlyEpsilon] relation*)
		(*NB: implementation only works if the two \[CurlyEpsilon]'s do not contract*)
		pairs= Subsets[Position[op, _\[CurlyEpsilon], All], {2}];
		opIdentities= opIdentities~ Join~ Table[
				eps= Extract[op, pair];
				Catch[
					If[eps[[1, 1, 1]] =!= Bar@ eps[[2, 1, 1]], Throw@ Nothing;];
					newOp= Delete[op, pair];
					indices= MapAt[Bar, List@@@ eps, {1, ;;}];
					op+ (newOp/. Thread[Rule@@ indices])-
						(newOp/. Thread[Rule@@ MapAt[Reverse, indices, {1}]])
				]
			, {pair, pairs}];
				
		(*Account for H.c.*)
		If[OptionValue@ IncludeHc,
			AppendTo[opIdentities, op- ConjugateOperator@ op];
		];
		
		(*SU(2) relation for one operator*)
		groups= Tally@ Cases[op, T[Index[gr_@ adj, _], __Index]:> gr, All];
		opIdentities= opIdentities~ Join~ Flatten@ Table[
			{group, genCount}= group;
			Catch@ Switch[{$flavorSymmetries[flavorSym, Groups, group], genCount}
			,{SU@ 2, 1},
				(*Do not apply identities if \[CurlyEpsilon] relations can be applied*)
				If[Length@ Cases[op, \[CurlyEpsilon][Index[Bar@ _group, _], _], All] > 0 && 
						Length@ Cases[op, \[CurlyEpsilon][Index[_group, _], _], All] > 0,
					Throw@ Nothing;
				];
				genInds= First@ Cases[op, x:T[Index[_group, _], __]:> List@@ x, All];
				(*Trace of generator vanishes*)
				If[genInds[[2]]=== Bar@ genInds[[3]], Throw@ op; ];
				indices= DeleteDuplicates@ 
					Cases[op, Index[(group@ fund|Bar@ group@ fund), ind_]:> Index[group@ fund, ind], All];
				indices= DeleteCases[indices, genInds[[2]]|Bar@ genInds[[3]]];
				(*Implement all instances of identities with T^{ai}_{j} delta^{k}_{l} *)
				Table[
					deltaInds= Bar@ Reverse@ {deltaInds, Index[Bar@ group@ fund, Unique@ $d]};
					newOp= DeleteCases[op, T[Index[_group, _], __], All]/. deltaInds[[2]]-> Bar@ deltaInds[[1]];  
					op + 
						Insert[newOp/. {Bar@ genInds[[2]]-> genInds[[3]]}, T[genInds[[1]], Sequence@@ deltaInds], {-1, -1}] -
						Insert[newOp/. {Bar@ genInds[[3]]-> deltaInds[[1]]}, T[genInds[[1]], genInds[[2]], deltaInds[[2]]], {-1, -1}] - 
						Insert[newOp/. {Bar@ genInds[[2]]-> deltaInds[[2]]}, T[genInds[[1]], deltaInds[[1]], genInds[[3]]], {-1, -1}]
				, {deltaInds, indices}]
			,_,
				Nothing
			]
		, {group, groups}];
		
		Sow@ MatchOperatorPatterns@ opIdentities; 
		opID++; len= Length@ $operatorIdentifiers;
	] ][[2, 1]];
	identities= DeleteCases[identities, 0];
	If[Length@ identities === 0,
		Return@ {};
	];
	
	(*Order the operators according to IBPScore*)
	ordering=  -OperatorScore[#, flavorSym]&/@ $operatorIdentifiers[[;;, 2]]; 
	ordering= Ordering@ ordering;
	ops= OpID/@ ordering;
	(*revOrdering= Ordering@ ordering;*)
	
	(*Use row reduction to generate a list of substitution rules for redundant operators*)
	identities= (identities/. OpID@ i_:> UnitVector[len, i])[[;;, ordering]];
	identities= DeleteCases[RowReduce@ identities, {0..}];
	Table[
		lhs= OpID@ ordering[[First@ FirstPosition[identity, 1]]]; 
		rhs= lhs- identity . ops;
		lhs-> rhs
	, {identity, identities}]
];


(* ::Text:: *)
(*Scoring the operators with with highest score the most undesirable *)


OperatorScore[op_Operator, flavorSym_]:=Module[{score},
	score= 10* Length@ op[[-1]]; (*Penalize number of cgs*)
	score-= If[ReducibleOperatorQ[op, flavorSym], 100, 0];
	score+= If[MatchQ[_l12| _e12| _q12| _u12| _d12]@ op[[1, -1]], 2, 0];
	score+= If[MatchQ[Bar[_l12| _e12| _q12| _u12| _d12]]@ op[[1, -2]], 1, 0]
]


(* ::Subsection:: *)
(*Determine operator basis *)


(* ::Subsubsection::Closed:: *)
(*Basis for a single Operator--Spurion combination*)


(* ::Text:: *)
(*Basis given Spurions and SMEFT operator*)


OperatorSpurionBasis[SMEFTop_, flavorSym_, spurions_Spur]:= Module[{singlets, identities, selfConjugate},
	(*Find all singlet contractions of fields and spurions*)
	singlets = ConstructSinglets[SMEFTop, flavorSym, spurions];
	If[Length@ singlets === 0, Return@ {};];
	(*Checks if the operator-spurion combination is self conbjugate*)
	selfConjugate= Bar@ spurions === spurions && !$smeftOperators[SMEFTop, UniqueHc];
	(*Remove operators that can be simplified away*)
	ResetOperatorPatterns[];
	singlets= MatchOperatorPatterns@ singlets;
	identities= ConstructOperatorIdentities[SMEFTop, flavorSym, IncludeHc-> selfConjugate];
	singlets= Complement[Keys@ $operatorIdentifiers, identities[[;;,1]]]/. $operatorIdentifiers;
	(*singlets= Complement[singlets, identities[[;;,1]]]/. $operatorIdentifiers;*)
	
	(*Remove operators, where a set of spurions form a singlet*)
	DeleteCases[singlets, _?(ReducibleOperatorQ[flavorSym])]
]


(* ::Subsubsection::Closed:: *)
(*Spurion combinations*)


(* ::Text:: *)
(*Determines all combination of spurions given flavor symmetry and counting order*)


SpurionCombinations[flavorSym_, ord_Integer]:= Module[{spurCases, counting, spurions, n, noCCcases},
	counting=Lookup[$flavorSymmetries[flavorSym], SpurionCounting, {}];
	spurions= DeleteDuplicates@ Join[Keys@ counting, Bar/@ Keys@ counting];
	spurCases= Join@@ Table[Tuples[spurions, n], {n, 0, ord}];
	spurCases= Select[spurCases, Plus@@ (#/. Bar-> Identity/. counting) <= ord &];
	(*All spurion cases satisfying the counting*)
	spurCases= Spur@@@ spurCases //DeleteDuplicates;
	(*Removing the complex conjugates*)
	noCCcases= DeleteDuplicatesBy[(Sort@ {#, Bar@ #}&)/@ spurCases, Last][[;;, 1]];
	{noCCcases, spurCases}
]


(* ::Subsubsection::Closed:: *)
(*Operator basis for the full symmetry*)


(* ::Text:: *)
(*Returns all operators of a symmetry for all SMEFT-Spurion combinations*)


Options@ OperatorBasis= {
		SpurionCount-> 3,
		SMEFToperators-> All
	};
OperatorBasis[flavorSym_, OptionsPattern[]]:= Module[{spurionCombinations, smeftOps, 
		smeftOp, spurs},
	spurionCombinations= SpurionCombinations[flavorSym, OptionValue@ SpurionCount];
	smeftOps= If[OptionValue@ SMEFToperators === All, 
		Keys@ $smeftOperators, OptionValue@ SMEFToperators];
	
	Association@ Table[smeftOp->
		DeleteCases[Merge[Table[<|(spur/. Bar-> Identity)->
			OperatorSpurionBasis[smeftOp, flavorSym, spur]/. 
			op_Operator? (Not@* SelfConjugateQ[smeftOp])-> PlusHc@ op|>,
				{spur, spurionCombinations[[If[$smeftOperators[smeftOp, UniqueHc], 2, 1]]]}], Flatten], {}]
	,{smeftOp, smeftOps}]
]


(* ::Subsubsection::Closed:: *)
(*Basis count*)


(* ::Text:: *)
(*Returns the counting of the operators in the basis *)


Options@ BasisCount= Join[Options@ OperatorBasis,{}];
BasisCount[flavorSym_, opt:OptionsPattern[]]:= Module[{newOpts, basis, count},
	newOpts= FilterRules[{opt}, Options@ OperatorBasis];
	basis= OperatorBasis[flavorSym, Sequence@@ newOpts];
	count= Map[{Length@ #, Count[#, _PlusHc]}&, basis, {2}];
	(*Account for operator multiplicity*)
	count= Association@ KeyValueMap[Rule[#1, Lookup[$smeftOperators@ #1, Multiplicity, 1] #2]&, count];
	count@ Total= Merge[List@@ count, Total];
	count
]


(* ::Subsection::Closed:: *)
(*Tables *)


(* ::Text:: *)
(*Outputs counting table based on the basis  *)


Options@ CountingTable= Join[Options@ BasisCount, {}];
CountingTable[flavorSym_, opt:OptionsPattern[]]:= Module[{count, newOpts, spurions, firstRow, 
		lastRow,table,classLabel, spur},
	newOpts= FilterRules[{opt}, Options@ BasisCount];
	count= BasisCount[flavorSym, Sequence@@ newOpts];
	
	(*Organize spurion columns*)
	spurions= DeleteDuplicates@ Flatten[Keys/@ List@@ count];
	spurions= SortBy[spurions, (Plus@@ #/. $flavorSymmetries[flavorSym, SpurionCounting]&)];
	firstRow= {flavorSym, SpanFromLeft}~ Join~ spurions/. Spur:> "\[ScriptCapitalO]"@*Times;
	
	table= Flatten[Table[
		classLabel= True;
		Table[
			Catch[
				If[Length@ Lookup[count, smeftOp, <||>] === 0, Throw@ Nothing;];
				Join[
					{If[classLabel, classLabel= False; class, SpanFromAbove], smeftOp}
					,	
				Table[
						Lookup[count@ smeftOp, spur, "\t"]/. List-> CountPairToString
					, {spur, spurions}]
				]
			]
		, {smeftOp, $operatorClasses@ class}]	
	, {class, Keys@ $operatorClasses}],1];
	
	lastRow= Join[{"Total", SpanFromLeft}, Table[
				Lookup[count@ Total, spur, "\t"]/. List-> CountPairToString
			, {spur, spurions}] ];
	
	Grid[Join[{firstRow}, table, {lastRow}], Dividers-> {All, All}]
	
];
CountPairToString[a_, b_]:= If[a === 0, "", ToString@ a]<> "\t"<> If[b === 0, "", ToString@ b];
