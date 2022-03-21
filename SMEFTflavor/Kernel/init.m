(* ::Package:: *)

(* ::Title:: *)
(*Initialization [SMEFTflavor`]*)


(* Restrict the runnable version number of Mathematica *)
If[!OrderedQ[{11.0, 0}, {$VersionNumber, $ReleaseNumber}], 
  Print["SMEFTflavor` requires Mathematica 11.0.0 or later."];
  Abort[]
]


(* Loading the package *)
If[MemberQ[$Packages,"SMEFTflavor`"],
	(* Avoid double loading the package *)
	Print@ Style["The package SMEFTflavor` is already loaded. Restart the kernel to reload it.", RGBColor[.8,.4706,0.2573]],
	
	(* Loading the file if its not already loaded*)
	$SMEFTflavorDirectory= DirectoryName[$InputFileName, 2];
	(* Loading *)
	Print["Loading SMEFTflavor`..."];
	(* Load, but abort if there is a message *)
	Check[
		Get@ FileNameJoin@ {$SMEFTflavorDirectory, "SMEFTflavor.m"},
		Print@Style["Loading failed!", RGBColor[.6, .0706, 0.1373]];
		Abort[]
	];
	Print[
	"by Anders Eller Thomsen, Ajdin Palavri\[CAcute], and Admir Greljo.\n",
	"Reference: ", Hyperlink["arXiv:2203.09561","https://arxiv.org/abs/2203.09561"],"\n",
	"Website: ", Hyperlink["https://github.com/aethomsen/SMEFTflavor","https://github.com/aethomsen/SMEFTflavor"]
	];
];
