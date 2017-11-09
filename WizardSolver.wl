(* ::Package:: *)

(* ::Text:: *)
(*Version 1.3.6*)
(*New Feature: Statistic test now supports more than one pattern.*)


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
(* Function tests *)
GP[a_,b_,c_]:=(a*c==b^2);                 (* Geometric Progression *)
PT[a_,b_,c_]:=(a^2+b^2==c^2);             (* pythagorean Triple    *)
DB[a_,b_]:=(Mod[a,b]==0);                 (* Divisible By          *)
GPdict=toLetter/@#&/@Select[Tuples[Range@26,3],GP@@#&];
PTdict=toLetter/@#&/@Select[Tuples[Range@26,3],PT@@#&];


PossibleWords[format_,testlist_]:=Module[
	{
		i,j,k,               (* loop variables *)
		count,               (* loop counter *)
		
		letter={},           (* all possible letters *)
		ltr=0,               (* the length of the word *)
		identifier,          (* a symbol for matching *)
		inverted,            (* inverted meaning of a pattern *)
		
		test,tst,            (* a list of tests and their amount *)
		expression,          (* equation test: expression *)
		pattern,number,      (* statistic test: pattern and number *)
		patNorm,patInv,      (* statistic test: normal and inverted pattern *)
		function,            (* function test: functions *)
		position,            (* sequence numbers in a test *)
		failed,              (* if failing to pass the test *)
		temp,                (* a variable for temporary use *)
		
		do=True,             (* \:662f\:5426\:7ee7\:7eed\:5faa\:73af *)
		next,                (* \:8bb0\:5f55\:7ffb\:9875\:5730\:5740 *)
		pointer,             (* \:8bb0\:5f55\:6bcf\:4e2a\:4f4d\:7f6e\:5c1d\:8bd5\:5230\:7684\:5b57\:6bcd *)
		range,               (* \:8bb0\:5f55\:6bcf\:4e2a\:4f4d\:7f6e\:7684\:53ef\:80fd\:5b57\:6bcd\:6570 *)
		ordL,invL,id,        (* \:6700\:4f18\:7f6e\:6362\:ff0c\:5176\:9006\:6620\:5c04\:ff0c\:6052\:7b49\:6620\:5c04 *)
		ordT,                (* order of tests *)
		match,word,          (* \:53ef\:80fd\:7684\:5b57\:6bcd\:7ec4\:5408\:ff0c\:6784\:6210\:7684\:8bcd *)
		qualified,nxt,       (* \:662f\:5426\:6ee1\:8db3\:6761\:4ef6\:ff0c\:7ffb\:9875\:5730\:5740 *)
		
		answer={}	        (* a list of possible words *)	
	},
	
	(* A rough matching *)
	For[i=1,i<=StringLength@format,i++,
		identifier=StringTake[format,{i}];
		Switch[identifier,
			"#"|"@"|"*",                                (* letter *)
				ltr++;
				inverted=False;
				AppendTo[letter,patRule[identifier]],
			"!",                                        (* opposition *)
				inverted=True,
			_,                                          (* pattern *)
				If[inverted,
					letter[[ltr]]=Complement[Letters,patRule[identifier]]\[Intersection]letter[[ltr]],
					letter[[ltr]]=patRule[identifier]\[Intersection]letter[[ltr]]
				];
		];
	];
	
	(* Manipulating the tests *)
	test=StringSplit[testlist,";"];
	tst=Length@test;
	failed={};
	position={};
	For[k=1,k<=tst,k++,
		Which[
			StringContainsQ[test[[k]],"["],                                           (* \:51fd\:6570\:6761\:4ef6 *)
				function=StringSplit[test[[k]],"["~~__~~"]"][[1]];
				AppendTo[failed,ToExpression["!"<>
					StringReplace[test[[k]],b:NumberString:>"(toCode@#[["<>b<>"]])"]
				<>"&"]];
				AppendTo[position,ToExpression/@StringCases[test[[k]],NumberString]];
				Switch[function,
					"GP",
						Do[letter[[position[[k,l]]]]=letter[[position[[k,l]]]]\[Intersection]GPdict\[Transpose][[l]],{l,3}];
				],
			StringContainsQ[test[[k]],"="],                                           (* \:7b49\:5f0f\:6761\:4ef6 *)
				AppendTo[failed,ToExpression["!"<>
					StringReplace[test[[k]],{
						"&"~~b:NumberString:>"(toCode@#[["<>b<>"]])",
						"="->"=="}]
					<>"&"]];
				AppendTo[position,ToExpression/@StringCases[test[[k]],"&"~~b:NumberString:>b]];
				If[StringMatchQ[test[[k]],("&"~~NumberString~~"=")..~~"&"~~NumberString],
					temp=Intersection@@letter[[position[[k]]]];
					Do[letter[[position[[k,l]]]]=temp,{l,Length@position[[k]]}];
				],
			StringContainsQ[test[[k]],"~"],                                           (* \:6784\:8bcd\:6761\:4ef6 *)
				AppendTo[position,ToExpression/@StringSplit[test[[k]],"~"]];
				Which[
					StringTake[test[[k]],{1}]=="~",
					AppendTo[failed,!MemberQ[postDict,StringJoin@#[[position[[k]]]]]&],
					StringTake[test[[k]],{-1}]=="~",
					AppendTo[failed,!MemberQ[preDict,StringJoin@#[[position[[k]]]]]&],
					True,
					AppendTo[failed,!DictionaryWordQ@StringJoin@#[[position[[k]]]]&]
				],
			StringContainsQ[test[[k]],":"],                                           (* \:7edf\:8ba1\:6761\:4ef6 *)
				pattern=StringSplit[test[[k]],":"][[1]];
				number=ToExpression@StringSplit[test[[k]],":"][[2]];
				AppendTo[position,Range@ltr];
				If[StringContainsQ[pattern,"!"],
					{patNorm,patInv}=StringSplit[pattern,"!"];
					AppendTo[failed,
						Count[#,char_/;MemberQ[
							Intersection[Intersection@@(patRule/@Characters@patNorm),
							Complement[Letters,Union@@(patRule/@Characters@patInv)]],
						char]]!=number&
					],
					AppendTo[failed,
						Count[#,char_/;MemberQ[
							Intersection@@patRule/@Characters@pattern,
						char]]!=number&
					]
				];
		];
	];
	If[$debug,Print[Column@failed];Print[position];];
	
	(* Brute force *)
	count=0;
	id=Range@ltr;
	invL=id;
	next={};
	ordL=Reverse@DeleteDuplicates@Flatten@SortBy[Join[position,{id}],Length];
	For[i=1,i<=ltr,i++,
		invL[[ordL[[i]]]]=i;
	];
	For[k=1,k<=tst,k++,
		AppendTo[next,Min@invL[[position[[k]]]]];
	];
	range=Length/@letter[[ordL]];
	pointer=ConstantArray[1,ltr];
	ordT=SortBy[Range@tst,-next@ordL];
	If[$debug,
		Print[ordL];
		Print[range];
		Print[next];
		Print[ordT];
	];
	While[do,
		qualified=True;nxt=1;
		match=letter[[#,pointer[[invL[[#]]]]]]&/@id;
		For[k=1,k<=tst,k++,
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
		While[j<=ltr&&pointer[[j]]==range[[j]],
			pointer[[j]]=1;
			j++;
		];
		If[j>ltr,do=False,pointer[[j]]++];
		count++;
	];
	If[$debug,Print[count]];
	answer
];


(* ::Code:: *)
(*AbsoluteTiming@PossibleWords["#p@#4*s#@#!e#","&4+2=&5;GP[3,7,8];~6~7~8"]*)


(* ::Code:: *)
(*AbsoluteTiming@PossibleWords["#@#c@#45@#@","&3=&7;ap!2:2;1~2~3~4~;~8~7"]*)
