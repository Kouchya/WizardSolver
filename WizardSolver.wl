(* ::Package:: *)

(* ::Text:: *)
(*Version 1.3.5*)
(*\:5f15\:5165\:ff1a\:6784\:8bcd\:6761\:4ef6\:4e2d\:52a0\:5165\:4e86\:524d\:540e\:7f00\:3002*)
(*\:4f18\:5316\:ff1a\:76f8\:7b49\:5173\:7cfb\:7684\:9884\:5904\:7406\:3002*)
(*\:4f18\:5316\:ff1a\:5224\:65ad\:65f6\:7684\:8fdb\:6b65\:673a\:5236\:3002*)
(*\:4f18\:5316\:ff1aGP\:548cPT\:7684\:4f18\:5316\:3002*)


(* \:521d\:59cb\:5316 *)
$Language="English";
$debug=False;
$path=NotebookDirectory[]<>"Library\\";
Letters=CharacterRange["A","Z"];
Vowels={"A","E","I","O","U"};
Consonants=Complement[Letters,Vowels];
PatRule=Characters@
<|
	"*"->StringJoin@Letters,
	"@"->StringJoin@Vowels,
	"#"->StringJoin@Consonants,
	"a"->"ABCDEHIKMOTUVWXY",               (* Axial Symmetry   *)
	"c"->"HINOSXZ",                        (* Central Symmetry *)
	"l"->"ABDOPQR",                        (* With Loop        *)   
	"u"->"BCDGIJLMNOPRSUVWZ",              (* Unicursal        *)
	"p"->"BCEGKMQSW",                      (* Prime Number     *)
	"d"->"DFHIJLNOPRTUVXYZ",               (* Divisible Number *)
	"s"->"ADIPY",                          (* Square Number    *)
	"f"->"DHILPRTXY",                      (* Square-Free      *)
	"e"->"BCFHIKNOPSUVWY",                 (* Chemical Element *)
	"2"->"BDFHJLNPRTVXZ",
	"3"->"CFILORUX",
	"4"->"DHLPTX",
	"5"->"EJOTY"
|>;
toCode=ToCharacterCode[#][[1]]-64&;
toLetter=FromCharacterCode[#+64]&;
predict=StringSplit@Import[$path<>"Prefix.txt"];
PoFdict=StringSplit@Import[$path<>"Postfix.txt"];
GPdict=toLetter/@#&/@Select[Tuples[Range@26,3],#[[1]]#[[3]]==#[[2]]^2&];
PTdict=toLetter/@#&/@Select[Tuples[Range@26,3],#[[1]]^2+#[[2]]^2==#[[3]]^2&];


(* \:51fd\:6570\:6761\:4ef6 *)
GP[a_,b_,c_]:=(a*c==b^2);                 (* \:7b49\:6bd4\:6570\:5217 *)
PT[a_,b_,c_]:=(a^2+b^2==c^2);             (* \:52fe\:80a1\:6570 *)
DB[a_,b_]:=(Mod[a,b]==0);                 (* \:6574\:9664\:6027 *)


PossibleWords[format_,testlist_]:=Module[
	{
		i,j,k,               (* \:5faa\:73af\:53d8\:91cf *)
		count,               (* \:5faa\:73af\:6b21\:6570(\:7528\:4e8e\:8c03\:8bd5) *)
		
		letter={},ltr=0,     (* \:6bcf\:4e00\:4f4d\:7684\:53ef\:80fd\:5b57\:6bcd\:ff0c\:5b57\:6bcd\:7684\:6570\:91cf *)
		identifier,          (* \:67d0\:4e00\:4f4d\:5339\:914d\:7b26 *)
		inversed,            (* \:662f\:5426\:53cd\:76f8\:5339\:914d *)
		
		test,tst,            (* \:6d4b\:8bd5\:5217\:8868\:ff0c\:6d4b\:8bd5\:7684\:6570\:91cf *)
		expression,          (* \:7b49\:5f0f\:6761\:4ef6\:4e2d\:7684\:8868\:8fbe\:5f0f *)
		pattern,number,      (* \:7edf\:8ba1\:6761\:4ef6\:4e2d\:7684\:6a21\:5f0f\:ff0c\:6570\:76ee *)
		function,            (* \:6761\:4ef6\:4e2d\:51fa\:73b0\:7684\:51fd\:6570 *)
		position,            (* \:6761\:4ef6\:4e2d\:51fa\:73b0\:7684\:5e8f\:53f7 *)
		next,                (* \:8bb0\:5f55\:7ffb\:9875\:5730\:5740 *)
		failed,              (* \:5224\:5b9a\:662f\:5426\:9519\:8bef *)
		intersection,
		
		do=True,             (* \:662f\:5426\:7ee7\:7eed\:5faa\:73af *)
		pointer,             (* \:8bb0\:5f55\:6bcf\:4e2a\:4f4d\:7f6e\:5c1d\:8bd5\:5230\:7684\:5b57\:6bcd *)
		range,               (* \:8bb0\:5f55\:6bcf\:4e2a\:4f4d\:7f6e\:7684\:53ef\:80fd\:5b57\:6bcd\:6570 *)
		ord,inv,id,          (* \:6700\:4f18\:7f6e\:6362\:ff0c\:5176\:9006\:6620\:5c04\:ff0c\:6052\:7b49\:6620\:5c04 *)
		match,word,          (* \:53ef\:80fd\:7684\:5b57\:6bcd\:7ec4\:5408\:ff0c\:6784\:6210\:7684\:8bcd *)
		qualified,nxt,       (* \:662f\:5426\:6ee1\:8db3\:6761\:4ef6\:ff0c\:7ffb\:9875\:5730\:5740 *)
		
		answer={}	        (* \:7b54\:6848\:5217\:8868 *)	
	},
	
	(* \:7c97\:5339\:914d *)
	For[i=1,i<=StringLength@format,i++,
		identifier=StringTake[format,{i}];
		Switch[identifier,
			"#"|"@"|"*",                                (* \:5b57\:6bcd *)
				ltr++;
				inversed=False;
				AppendTo[letter,PatRule[identifier]],
			"!",                                        (* \:53cd\:76f8 *)
				inversed=True,
			_,                                          (* \:6a21\:5f0f *)
				If[inversed,
					letter[[ltr]]=Complement[Letters,PatRule[identifier]]\[Intersection]letter[[ltr]],
					letter[[ltr]]=PatRule[identifier]\[Intersection]letter[[ltr]]
				];
		];
	];
	
	(* \:6d4b\:8bd5\:51fd\:6570 *)
	test=StringSplit[testlist,";"];
	tst=Length@test;
	failed={};
	position={};
	j=0;
	For[k=1,k<=tst,k++,
		j++;
		Which[
			StringContainsQ[test[[k]],"["],                                           (* \:51fd\:6570\:6761\:4ef6 *)
				function=StringSplit[test[[k]],"["~~__~~"]"][[1]];
				Switch[function,
					_,
					AppendTo[failed,ToExpression["!"<>
						StringReplace[test[[k]],b:NumberString:>"(toCode@#[["<>b<>"]])"]
					<>"&"]];
					AppendTo[position,ToExpression/@StringCases[test[[k]],NumberString]];
				],
			StringContainsQ[test[[k]],"="],                                           (* \:7b49\:5f0f\:6761\:4ef6 *)
				AppendTo[failed,ToExpression["!"<>
					StringReplace[test[[k]],{
						"&"~~b:NumberString:>"(toCode@#[["<>b<>"]])",
						"="->"=="}]
					<>"&"]];
				AppendTo[position,ToExpression/@StringCases[test[[k]],"&"~~b:NumberString:>b]];
				If[StringMatchQ[test[[k]],("&"~~NumberString~~"=")..~~"&"~~NumberString],
					intersection=Intersection@@letter[[position[[j]]]];
					Do[letter[[position[[j,l]]]]=intersection,{l,Length@position[[j]]}];
				]
			StringContainsQ[test[[k]],"~"],                                           (* \:6784\:8bcd\:6761\:4ef6 *)
				AppendTo[position,ToExpression/@StringSplit[test[[k]],"~"]];
				AppendTo[failed,!DictionaryWordQ@StringJoin@#[[position[[k]]]]&];
			StringContainsQ[test[[k]],":"],                                           (* \:7edf\:8ba1\:6761\:4ef6 *)
				pattern=StringSplit[test[[k]],":"][[1]];
				number=ToExpression@StringSplit[test[[k]],":"][[2]];
				AppendTo[position,Range@ltr];
				AppendTo[failed,Count[#,char_/;MemberQ[PatRule[pattern],char]]!=number&];
		];
	];
	tst=j;
	If[$debug,Print[Column@failed];Print[position];];
	
	(* \:4f9d\:6b21\:5339\:914d *)
	count=0;
	id=Range@ltr;
	inv=id;
	next={};
	ord=Reverse@DeleteDuplicates@Flatten@SortBy[Join[position,{id}],Length];
	For[i=1,i<=ltr,i++,
		inv[[ord[[i]]]]=i;
	];
	For[k=1,k<=tst,k++,
		AppendTo[next,Min@inv[[position[[k]]]]];
	];
	range=Length/@letter[[ord]];
	pointer=ConstantArray[1,ltr];
	If[$debug,Print[ord];Print[range]];
	While[do,
		qualified=True;nxt=1;
		match=letter[[#,pointer[[inv[[#]]]]]]&/@id;
		For[k=1,k<=tst,k++,
			If[failed[[k]]@match,
				qualified=False;
				nxt=Max[nxt,next[[k]]];
			]
		];
		word=StringJoin@@match;
		If[qualified&&DictionaryWordQ@word,
			AppendTo[answer,ToLowerCase@word];
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
(*AbsoluteTiming@PossibleWords["#p@#s@#@##s","&1=&5;&2=&4;GP[3,7,8]"]*)


(* ::Code:: *)
(*Timing@PossibleWords["#@#c@#@#@","&2=&4=&6;&3=&7;&2=&3+1"]*)
