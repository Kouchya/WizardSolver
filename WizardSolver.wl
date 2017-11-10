(* ::Package:: *)

(* ::Text:: *)
(*Version 1.3.6*)
(*New Feature: Statistic test now supports more than one pattern*)
(*New Feature: AP(Arithmetic Progression)*)
(*Optimization: optimized the identification of patterns*)


(* Initialization *)
$Language="English";
$debug=False;
$path=NotebookDirectory[]<>"Library\\";
toCode=ToCharacterCode[#][[1]]-96&;
toLetter=FromCharacterCode[#+96]&;
(* Dictionary *)
Letters=CharacterRange["a","z"];
preDict=StringSplit@Import[$path<>"Prefix.txt"];
postDict=StringSplit@Import[$path<>"Postfix.txt"];
patDict=StringSplit[Import[$path<>"Pattern.txt"],"\n"];
patName=StringTake[Take[patDict,{1,-2,2}],1];
patLetter=Take[patDict,{2,-1,2}];
patRule=Characters@Association[#[[1]]->#[[2]]&/@Transpose@{patName,patLetter}];
GPdict=toLetter/@#&/@Select[Tuples[Range@26,3],#[[1]]#[[3]]==#[[2]]^2&];
PTdict=toLetter/@#&/@Select[Tuples[Range@26,3],#[[1]]^2+#[[2]]^2==#[[3]]^2&];
(* Functions *)
Candidate[format_]:=Module[{patNorm,patInv},
	If[StringContainsQ[format,"!"],
		{patNorm,patInv}=Characters/@StringSplit[format,"!"];
		Intersection[Intersection@@(patRule/@patNorm),Complement[Letters,Union@@(patRule/@patInv)]],
		Intersection@@patRule/@Characters@format
	]
];


PossibleWords[format_,testlist_]:=Module[
	{
		i,j,k,               (* loop variables *)
		count,               (* loop counter *)		
		letter,              (* all possible letters *)
		length,              (* the length of the word *)
		id,                  (* identity mapping of letters *)
		
		test,testN,          (* a list of tests and their amount *)
		expression,          (* equation test: expression *)
		pattern,number,      (* statistic test: pattern and number *)
		function,            (* function test: functions *)
		position,            (* sequence numbers in a test *)
		failed,              (* if failing to pass the test *)
		letTmp,              (* a variable for temporary use *)
		
		do=True,             (* whether the search is to continue *)
		next,                (* record address for the next trial *)
		pointer,             (* record letter address in an attempt *)
		range,               (* record the amount of possible letters *)
		ordL,                (* order of letter search *)
		invL,                (* inversed order of letter search *)
		ordT,                (* order of tests *)
		match,               (* an untested combination of letters *)
		word,                (* an untested word *)
		qualified,           (* whether the word is qualified *)
		nxt,                 (* the address for the next trial *)
		
		answer={}	        (* a list of possible words *)	
	},
	
	letter=Candidate/@StringSplit[StringReplace[format,p:"@"|"#"|"*":>" "<>p]];
	length=StringCount[format,"@"|"#"|"*"];
	id=Range@length;
	
	(* Manipulating the tests *)
	test=StringSplit[testlist,";"];
	j=0;
	failed={};
	position={};
	For[k=1,k<=Length@test,k++,
		j++;
		Which[
			StringContainsQ[test[[k]],"["],                                           (* function test *)
				function=StringSplit[test[[k]],"["~~__~~"]"][[1]];
				AppendTo[position,ToExpression/@StringCases[test[[k]],NumberString]];
				Switch[function,
					"GP",
						AppendTo[failed,!MemberQ[GPdict,#[[position[[k]]]]]&];
						Do[letter[[position[[k,l]]]]=Intersection[letter[[position[[k,l]]]],Transpose[GPdict][[l]]],{l,3}],
					"AP",
						AppendTo[failed,#1+#3!=2#2&@@toCode/@#[[position[[k]]]]&],
					"PT",
						AppendTo[failed,!MemberQ[PTdict,#[[position[[k]]]]]&];
						Do[letter[[position[[k,l]]]]=Intersection[letter[[position[[k,l]]]],Transpose[PTdict][[l]]],{l,3}],
					"DB",
						AppendTo[failed,Mod[#1,#2]!=0&@@toCode/@#[[position[[k]]]]&];
				],
			StringContainsQ[test[[k]],"="],                                           (* equation test *)
				AppendTo[failed,ToExpression["!"<>
					StringReplace[test[[k]],{
						"&"~~b:NumberString:>"(toCode@#[["<>b<>"]])",
						"="->"=="}]
					<>"&"]];
				AppendTo[position,ToExpression/@StringCases[test[[k]],"&"~~b:NumberString:>b]];
				If[StringMatchQ[test[[k]],("&"~~NumberString~~"=")..~~"&"~~NumberString],
					letTmp=Intersection@@letter[[position[[k]]]];
					Do[letter[[position[[k,l]]]]=letTmp,{l,Length@position[[k]]}];
				],
			StringContainsQ[test[[k]],"~"],                                           (* word test *)
				AppendTo[position,ToExpression/@StringSplit[test[[k]],"~"]];
				Which[
					StringTake[test[[k]],{1}]=="~",
					AppendTo[failed,!MemberQ[postDict,StringJoin@#[[position[[k]]]]]&],
					StringTake[test[[k]],{-1}]=="~",
					AppendTo[failed,!MemberQ[preDict,StringJoin@#[[position[[k]]]]]&],
					True,
					AppendTo[failed,!DictionaryWordQ@StringJoin@#[[position[[k]]]]&]
				],
			StringContainsQ[test[[k]],":"],                                           (* statistic test *)
				pattern=StringSplit[test[[k]],":"][[1]];
				number=ToExpression@StringSplit[test[[k]],":"][[2]];
				AppendTo[position,Range@length];
				AppendTo[failed,Count[#,char_/;MemberQ[Candidate[pattern],char]]!=number&]
		];
	];
	testN=j;
	If[$debug,Print[Column@failed];Print["position:",position]];
	
	(* Brute force *)
	count=0;
	invL=id;
	next={};
	ordL=Reverse@DeleteDuplicates@Flatten@SortBy[Join[position,{id}],Length];
	For[i=1,i<=length,i++,
		invL[[ordL[[i]]]]=i;
	];
	For[k=1,k<=testN,k++,
		AppendTo[next,Min@invL[[position[[k]]]]];
	];
	range=Length/@letter[[ordL]];
	pointer=ConstantArray[1,length];
	ordT=SortBy[Range@testN,-next@ordL];
	If[$debug,
		Print["ordL:",ordL];
		Print["range:",range];
		Print["next:",next];
		Print["ordT:",ordT];
	];
	While[do,
		qualified=True;nxt=1;
		match=letter[[#,pointer[[invL[[#]]]]]]&/@id;
		For[k=1,k<=testN,k++,
			If[failed[[ordT[[k]]]]@match,
				qualified=False;
				nxt=Max[nxt,next[[k]]];
				Break;
			]
		];
		word=StringJoin@@match;
		If[qualified&&DictionaryWordQ@word,
			AppendTo[answer,word];
		];
		For[j=1,j<nxt,j++,
			pointer[[j]]=1;
		];
		j=nxt;
		While[j<=length&&pointer[[j]]==range[[j]],
			pointer[[j]]=1;
			j++;
		];
		If[j>length,do=False,pointer[[j]]++];
		count++;
	];
	If[$debug,Print["count:",count]];
	answer
];


(* ::Code:: *)
(*AbsoluteTiming@PossibleWords["#p@#4*s#@#!e#","&4+2=&5;GP[3,7,8];GP[6,5,4];~6~7~8"]*)


(* ::Code:: *)
(*AbsoluteTiming@PossibleWords["#@#c@#f!e@#@","&3=&7;&2=&4=&6;ap:2;1~2~3~4~"]*)


(* ::Code:: *)
(*AbsoluteTiming@PossibleWords["@###p@#c","PT[2,4,3];PT[4,2,3];DB[1,5];&6=14"]*)


(* ::Code:: *)
(*AbsoluteTiming@PossibleWords["#s#s#c@#p@#p#cp","&6=9;AP[1,6,3];AP[5,8,2]"]*)
