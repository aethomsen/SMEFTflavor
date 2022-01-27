(* ::Package:: *)

Package["SSC`"] 


(* ::Title:: *)
(*SSC`*)


(* ::Subtitle:: *)
(*Package to determine SMEFT operators for various symmetries and spurions*)


(* ::Section:: *)
(*Scoping & usage definitions*)


PackageScope["OptionsCheck"]


$SubscriptSize::usage= "Sets the global subscriptsize for QFTools formatting."


(* ::Subsection:: *)
(*Option checker*)


General::invalidopt = "Option `1` for function `2` received invalid value `3`.";
General::optexpectsval = "Option `1` for function `2` received invalid value `3`. A `4` is expected.";
OptionMessage[opt_, func_, val_] := Message[General::invalidopt, opt, func, val];


(*Messages for specific options/functions*)
OptionMessage[EpsilonExpand, func_, val_] := Message[General::optexpectsval, EpsilonExpand, func, val, "Boolean"];
OptionMessage[ToMasterIntegrals, func_, val_] := Message[General::optexpectsval, ToMasterIntegrals, func, val, "Boolean"];


(*Tests for specific options. Form expected is OptionTest[function, options] *)
OptionTest[_, ToMasterIntegrals] = BooleanQ;
OptionTest[_, EpsilonExpand] = BooleanQ;

Attributes @ OptionsCheck = {HoldFirst};
OptionsCheck @ func_[___, opts : OptionsPattern[]] :=
	And @@ (OptionTest[func, #1][#2] || OptionMessage[#1, func, #2] &) @@@ FilterRules[List[opts], Options @ func];

