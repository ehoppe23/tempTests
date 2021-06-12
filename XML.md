## XML grammar
Modification of `XML` grammar from [Egg](https://github.com/bruceiv/egg/blob/deriv/grammars/XML-u.egg) to test XML gramar.

```
package "XML"

Content        : { COMMENT | Element | REFERENCE | CHAR_DATA } ;

Document       : prolog Element repMisc0 ;
prolog 	       : optXMLDec repMisc0 ;
XMLDecl        : "<?xml" VersionInfo optEncodDecl optS "?>" ;
        optXMLDecl : [ XMLDecl ] ;
VersionInfo    : S "version" Eq quoVerNum ;
        quoVerNum : '\'' VersionNum '\''  | '\"' VersionNum '\"' ;

EncodingDecl   : S "encoding" Eq quoEncNam ;
        quoEncNam : '\'' EncName '\''  | '\"' EncName '\"' ;
        optEncodDecl : [ EncodingDecl ];

ATT_VALUE 	   : '\"' / '\"' ( any "^<&" | REFERENCE) ATT_VALUE ;
               | '\'' / '\'' (any "^<&" | REFERENCE) ATT_VALUE ;

REFERENCE      : ENTITY_REF | CHAR_REF ;
ENTITY_REF     : '&' NAME ';' ;
CHAR_REF       : "&#x" hex ';' | "&#" number ';' ;
hex : < number | any "abcdefABCDEF" > ;

Misc 	         : COMMENT | S ;
        repMisc0 : { Misc };
COMMENT        : "--" '>' / "<!--" . COMMENT ;

Element        : '<' NAME RepSAtt0 optS elemCloseAlts ;
        RepSAtt0 : { S Attribute } ;
        elemCloseAlts : '>' Content  "</" NAME optS '>' | "/>" ;

Attribute      : NAME optS '=' optS ATT_VALUE ;

Eq             : optS '=' optS ;
VersionNum 	   : < NAME_ CHAR > ;

NAME           : ( LETTER | any "_:" ) { NAME_CHAR } ;
NAME_CHAR      : LETTER | DIGIT | any "\-._:" ;
EncName 	   : LETTER {any LETTER DIGIT | any "._\-"}  ;
S              : < any "\t\r\n" > ;
        optS : [ S ] ;
LETTER         : letter ;
DIGIT          : number ;
CHAR_DATA      :  < any "^<&" > ; // Not 100%

```
