(* ::Package:: *)

(* ::Text:: *)
(*Version 1.3.3*)
(*\:4f7f\:7528\:4ea4\:6362\:7a77\:4e3e\:6b21\:5e8f\:7684\:65b9\:6cd5\:ff0c\:4f18\:5316\:8fd0\:884c\:65f6\:95f4\:3002*)


(* \:521d\:59cb\:5316 *)
$Language="English";
Letters=CharacterRange["A","Z"];
Vowels={"A","E","I","O","U"};
Consonants=Complement[Letters,Vowels];
PatRule=Characters@
<|
	"*"->StringJoin@Letters,
	"@"->StringJoin@Vowels,
	"#"->StringJoin@Consonants,
	"a"->"ABCDEHIKMOTUVWXY",               (* \:8f74\:5bf9\:79f0   *)
	"c"->"HINOSXZ",                        (* \:4e2d\:5fc3\:5bf9\:79f0 *)
	"r"->"ABDOPQR",                        (* \:542b\:5708     *)   
	"u"->"BCDGIJLMNOPRSUVWZ",              (* \:4e00\:7b14\:753b   *)
	"p"->"BCEGKMQSW",                      (* \:7d20\:6570     *)
	"s"->"ADIPY",                          (* \:5e73\:65b9\:6570   *)
	"f"->"DHILPRTXY",                      (* \:5e73\:65b9\:56e0\:5b50 *)
	"e"->"BCFHIKNOPSUVWY",                 (* \:5143\:7d20     *)
	"2"->"BDFHJLNPRTVXZ",
	"3"->"CFILORUX",
	"4"->"DHLPTX",
	"5"->"EJOTY"
|>;
LetterCode[char_]:=(ToCharacterCode[char]-64)[[1]];


PossibleWords[format_,testlist_]:=Module[
	{
		i,j,k,               (* \:5faa\:73af\:53d8\:91cf *)
		
		letter={},ltr=0,     (* \:6bcf\:4e00\:4f4d\:7684\:53ef\:80fd\:5b57\:6bcd\:ff0c\:5b57\:6bcd\:7684\:6570\:91cf *)
		identifier,          (* \:67d0\:4e00\:4f4d\:5339\:914d\:7b26 *)
		inversed,            (* \:662f\:5426\:53cd\:76f8\:5339\:914d *)
		
		test,tst,            (* \:6d4b\:8bd5\:5217\:8868\:ff0c\:6d4b\:8bd5\:7684\:6570\:91cf *)
		expression,          (* \:7b49\:5f0f\:6761\:4ef6\:4e2d\:7684\:8868\:8fbe\:5f0f *)
		pattern,number,      (* \:7edf\:8ba1\:6761\:4ef6\:4e2d\:7684\:6a21\:5f0f\:ff0c\:6570\:76ee *)
		position,            (* \:8bb0\:5f55\:6761\:4ef6\:4e2d\:51fa\:73b0\:7684\:5e8f\:53f7 *)
		next,                (* \:8bb0\:5f55\:7ffb\:9875\:5730\:5740 *)
		failed,              (* \:5224\:5b9a\:662f\:5426\:9519\:8bef *)
		
		do=True,             (* \:662f\:5426\:7ee7\:7eed\:5faa\:73af *)
		pointer,             (* \:8bb0\:5f55\:6bcf\:4e2a\:4f4d\:7f6e\:5c1d\:8bd5\:5230\:7684\:5b57\:6bcd *)
		range,               (* \:8bb0\:5f55\:6bcf\:4e2a\:4f4d\:7f6e\:7684\:53ef\:80fd\:5b57\:6bcd\:6570 *)
		ord,inv,idt,         (* \:7a77\:4e3e\:7f6e\:6362\:ff0c\:5176\:9006\:6620\:5c04\:ff0c\:6052\:7b49\:6620\:5c04 *)
		match,word,          (* \:53ef\:80fd\:7684\:5b57\:6bcd\:7ec4\:5408\:ff0c\:6784\:6210\:7684\:8bcd *)
		qualified,nxt,       (* \:662f\:5426\:6ee1\:8db3\:6761\:4ef6\:ff0c\:7ffb\:9875\:5730\:5740 *)
		
		answer={}	        (* \:7b54\:6848\:5217\:8868 *)	
	},
	
	(* \:7c97\:5339\:914d *)
	For[i=1,i<=StringLength@format,i++,
		identifier=StringTake[format,{i}];
		Switch[identifier,
			"#"|"@"|"*",                               (* \:5b57\:6bcd *)
				ltr++;
				inversed=False;
				AppendTo[letter,PatRule[identifier]],
			"!",                                       (* \:53cd\:76f8 *)
				inversed=True,
			_,                                         (* \:6a21\:5f0f *)
				If[inversed,
					letter[[ltr]]=Complement[Letters,PatRule[identifier]]\[Intersection]letter[[ltr]],
					letter[[ltr]]=PatRule[identifier]\[Intersection]letter[[ltr]]
				];
		];
	];
	
	(* \:6d4b\:8bd5\:51fd\:6570 *)
	test=StringSplit[testlist,";"];
	tst=Length@test;
	failed=ConstantArray[True&,tst];
	position=ConstantArray[Range@ltr,tst];
	next=ConstantArray[1,tst];
	For[k=1,k<=tst,k++,
		Which[
			StringContainsQ[test[[k]],"="],                               (* \:7b49\:5f0f\:6761\:4ef6 *)
				failed[[k]]=ToExpression["!"<>
					StringReplace[test[[k]],{
						"&"~~b:NumberString:>"(LetterCode@#[["<>b<>"]])",
						"="->"=="}]
					<>"&"];
				position[[k]]=ToExpression/@StringCases[test[[k]],"&"~~b:NumberString:>b];
			StringContainsQ[test[[k]],"~"],                               (* \:6784\:8bcd\:6761\:4ef6 *)
				position[[k]]=ToExpression/@StringSplit[test[[k]],"~"];
				failed[[k]]=!DictionaryWordQ@StringJoin@#[[position[[k]]]]&;
			StringContainsQ[test[[k]],":"],                               (* \:7edf\:8ba1\:6761\:4ef6 *)
				pattern=StringSplit[test[[k]],":"][[1]];
				number=ToExpression@StringSplit[test[[k]],":"][[2]];
				position[[k]]=Range@ltr;
				failed[[k]]=Count[#,char_/;MemberQ[PatRule[pattern],char]]!=number&;
		];
	];
	
	(* \:4f9d\:6b21\:5339\:914d *)
	idt=Range@ltr;
	inv=idt;
	ord=Reverse@DeleteDuplicates@Flatten@SortBy[Join[position,{idt}],Length];
	For[i=1,i<=ltr,i++,
		inv[[ord[[i]]]]=i;
	];
	For[k=1,k<=tst,k++,
		next[[k]]=Min@inv[[position[[k]]]];
	];
	range=Length/@letter[[ord]];
	pointer=ConstantArray[1,ltr];
	(*Print["ord=",ord];*)
	(*Print["next[[k]]=",next];*)
	(*Print["range=",range];*)
	While[do,
		qualified=True;nxt=1;
		match=letter[[#,pointer[[inv[[#]]]]]]&/@idt;
		For[k=1,k<=tst,k++,
			If[failed[[k]]@match,
				qualified=False;
				nxt=next[[k]];
				Break;
			]
		];
		word=StringJoin@@match;
		If[qualified&&DictionaryWordQ@word,
			AppendTo[answer,ToLowerCase@word];
		];
		(*Print[pointer,":",ToLowerCase@word,":",nxt];*)
		For[j=1,j<nxt,j++,
			pointer[[j]]=1;
		];
		j=nxt;
		While[j<=ltr&&pointer[[j]]==range[[j]],
			pointer[[j]]=1;
			j++;
		];
		If[j>ltr,do=False,pointer[[j]]++];
	];	
	(* \:7ed3\:679c *)
	answer
];


(* ::Code:: *)
(*Timing@PossibleWords["@#*p@s#a!u@p*s","p:3;1~2~4~5;&4=&6+&7"]*)


(* ::Code:: *)
(*Timing@PossibleWords["@#*p@s#a!u@p*s","p:3;1~2~4~5;&1=&3+2"]*)


(* ::Code:: *)
(*Timing@PossibleWords["@##@#@#","p:3;1~2~4~5;&4=&6+&7"]*)
(*Timing@PossibleWords["@###@",""]*)



