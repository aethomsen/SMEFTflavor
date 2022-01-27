(* ::Package:: *)

(* ::Title:: *)
(*Initialization [SSC`]*)


(* Restrict the runnable version number of Mathematica *)
If[!OrderedQ[{11.0, 0}, {$VersionNumber, $ReleaseNumber}], 
  Print["SSC` requires Mathematica 11.0.0 or later."];
  Abort[]
]


(* Loading the package *)
If[MemberQ[$Packages,"SSC`"],
	(* Avoid double loading the package *)
	Print@ Style["The package SSC` is already loaded. Restart the kernel to reload it.", RGBColor[.8,.4706,0.2573]],
	
	(* Loading the file if its not already loaded*)
	$SSCDirectory= DirectoryName[$InputFileName, 2];
	(* Loading *)
	Print["Loading SSC`..."];
	(* Load, but abort if there is a message *)
	Check[
		Get@ FileNameJoin@ {$SSCDirectory, "SSC.m"},
		Print@Style["Loading failed!", RGBColor[.6, .0706, 0.1373]];
		Abort[]
	];
	Print@ Style["Loading successful.", RGBColor[0.3,0.55,0.2]]
];
