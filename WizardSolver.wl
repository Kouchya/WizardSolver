(* ::Package:: *)

(* ::Text:: *)
(*(* Version 1.3.2 *)*)


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
		position,            (* \:8bb0\:5f55\:6761\:4ef6\:4e2d\:51fa\:73b0\:7684\:5e8f\:53f7 *)
		pattern,number,      (* \:7edf\:8ba1\:6761\:4ef6\:4e2d\:7684\:6a21\:5f0f\:ff0c\:6570\:76ee *)
		next={},             (* \:8bb0\:5f55\:7ffb\:9875\:5730\:5740 *)
		failed={},           (* \:5224\:5b9a\:662f\:5426\:9519\:8bef *)
		
		do=True,             (* \:662f\:5426\:7ee7\:7eed\:5faa\:73af *)
		pointer,             (* \:8bb0\:5f55\:6bcf\:4e2a\:4f4d\:7f6e\:5c1d\:8bd5\:5230\:7684\:5b57\:6bcd *)
		range,               (* \:8bb0\:5f55\:6bcf\:4e2a\:4f4d\:7f6e\:7684\:53ef\:80fd\:5b57\:6bcd\:6570 *)
		order,               (* \:8bb0\:5f55\:5c1d\:8bd5\:7684\:987a\:5e8f *)
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
				]
		]
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
				position[[k]]=ToExpression/@
					StringCases[test[[k]],
						"&"~~b:NumberString:>b
					];
				next[[k]]=Min@position[[k]],
			StringContainsQ[test[[k]],"~"],                               (* \:6784\:8bcd\:6761\:4ef6 *)
				position[[k]]=ToExpression/@StringSplit[test[[k]],"~"];
				failed[[k]]=!DictionaryWordQ@StringJoin@#[[position[[k]]]]&;
				next[[k]]=Min@position[[k]],
			StringContainsQ[test[[k]],":"],                               (* \:7edf\:8ba1\:6761\:4ef6 *)
				pattern=StringSplit[test[[k]],":"][[1]];
				number=ToExpression@StringSplit[test[[k]],":"][[2]];			
				failed[[k]]=Count[#,char_/;MemberQ[PatRule[pattern],char]]!=number&;
		]
	];
	
	(* \:4f9d\:6b21\:5339\:914d *)
	range=Length/@letter;
	pointer=ConstantArray[1,ltr];
	While[do,
		qualified=True;nxt=1;
		match=letter[[#,pointer[[#]]]]&/@Range@ltr;
		For[k=1,k<=tst,k++,
			(*If[match==Characters@"EXCITED"||match==Characters@"ASSAYED",
				Print[match,":",failed[[k]],":",failed[[k]]@match];
			]*)(* \:8c03\:8bd5 *)
			If[failed[[k]]@match,
				qualified=False;
				nxt=next[[k]];
				Break
			]
		];
		word=StringJoin@@match;
		If[qualified&&DictionaryWordQ@word,
			AppendTo[answer,ToLowerCase@word]
		];
		For[j=1,j<nxt,j++,
			pointer[[j]]=1
		];
		j=nxt;
		While[j<=ltr&&pointer[[j]]==range[[j]],
			pointer[[j]]=1;
			j++
		];
		If[j>ltr,do=False,pointer[[j]]++]
	];	
	(* \:7ed3\:679c *)
	answer
];



(* ::Code:: *)
(*Timing@PossibleWords["@#*p@s#a!u@p*s","5~4~6;p:3;&7=&4-&6"]*)
