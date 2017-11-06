(* ::Package:: *)

(* ::Input::Initialization:: *)
(* version 1.3.0 *)
$Language="English";
Letters=CharacterRange["A","Z"];
Vowels={"A","E","I","O","U"};
Consonants=Complement[Letters,Vowels];
PatRule=Characters@
<|
"*"->StringJoin@Letters,                   (* All Letters      *)
"@"->StringJoin@Vowels,                     (* Vowel            *)
"#"->StringJoin@Consonants,            (* Consonant        *)
"a"->"ABCDEHIKMOTUVWXY",                    (* Axial Symmetry   *)
"c"->"HINOSXZ",                                        (* Central Symmetry *)
"r"->"ABDOPQR",                                        (* With Ring        *)   
"u"->"BCDGIJLMNOPRSUVWZ",                 (* Unicursal        *)
"p"->"BCEGKMQSW",                                   (* Prime            *)
"s"->"ADIPY",                                            (* Square           *)
"f"->"DHILPRTXY",                                   (* Square-Free      *)
"e"->"BCFHIKNOPSUVWY",                        (* Element          *)
"2"->"BDFHJLNPRTVXZ",
"3"->"CFILORUX",
"4"->"DHLPTX",
"5"->"EJOTY"
|>;
PossibleWords[format_,testlist_]:=
Module[
{
(* \:5c40\:90e8\:53d8\:91cf *)
test={},tst,
position,testword,
pattern,number,
next={},
failed={},
letter={},ltr=0,
identifier,inversed,
i,j,k,
do=True,word,
qualified,nxt,
answer={},
pointer,order,range
},
(* \:751f\:6210\:7c97\:5339\:914d *)
For[i=1,i<=StringLength@format,i++,
identifier=StringTake[format,{i}];
Switch[identifier,
"#"|"@"|"*",                                                                    (* letter  *)
ltr++;
inversed=False;
AppendTo[letter,identifier/.PatRule],
"!",                                                                                          (* inverse *)
inversed=True,
_,                                                                                              (* pattern *)
If[inversed,
letter[[ltr]]=Complement[Letters,identifier/.PatRule]\[Intersection]letter[[ltr]],
letter[[ltr]]=(identifier/.PatRule)\[Intersection]letter[[ltr]]
]
]
];
(* \:559c\:95fb\:4e50\:89c1\:7684\:5931\:8d25 *)
test=StringSplit[testlist,";"];
tst=Length@test;
For[k=1,k<=tst,k++,
Which[
StringContainsQ[test[[k]],"~"],                                                                    (* word       *)
position=ToExpression/@StringSplit[test[[k]],"~"];
testword=StringJoin@StringPart[#,position]&;
AppendTo[failed,!DictionaryWordQ@testword@#&];
AppendTo[next,Min@position],
StringContainsQ[test[[k]],":"],                                                                    (* statistics *)
pattern=StringSplit[test[[k]],":"][[1]];
number=ToExpression@StringSplit[test[[k]],":"][[2]];
AppendTo[failed,StringCount[#,pattern/.PatRule]!=number&];
AppendTo[next,1];
]
];
(* \:4f9d\:6b21\:5339\:914d *)
range=Length/@letter;
pointer=ConstantArray[1,ltr];
While[do,
qualified=True;nxt=1;
word=StringJoin@@(letter[[#,pointer[[#]]]]&/@Range@ltr);
For[k=1,k<=tst,k++,
If[failed[[k]][word],
qualified=False;
nxt=next[[k]];
Break
]
];
If[qualified&&DictionaryWordQ@word,
AppendTo[answer,ToLowerCase@word]
];
For[j=1,j<nxt,j++,pointer[[j]]=1];
j=nxt;
While[j<=ltr&&pointer[[j]]==range[[j]],pointer[[j]]=1;j++];
If[j>ltr,do=False,pointer[[j]]++]
];
(* \:7ed3\:679c *)
answer
];


(* ::Code:: *)
(*Timing@PossibleWords["@p#*p@s#a!u@p*s","5~4~6;p:3"]*)
