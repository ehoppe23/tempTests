## JSON grammar
Modification of `JSON` grammar from [Egg](https://github.com/bruceiv/egg/blob/deriv/grammars/JSON-u.egg) to test JSON gramar.

```
package "JSON"

JSON : _ Object ;
Object : LBRACE optMem RBRACE ;
        optMem : < Members > ;
Members : Pair ComPair ;
        ComPair : {COMMA Pair} ;
Pair: String COLON Value ;
Array : LBRACKET OptElem RBRACKET ;
        OptElem : [ Elements ] ;
Elements : Value comVal ;
        comVal : {COMMA Value} ;
Value : String | Number | Object | Array | TRUE | FALSE | NUL ;
String : '\"' close _ ;
        close : '\"' / CHAR close ;
CHAR : upSlash | '\\' charCode ;
        charCode : escs | "u" HEX HEX HEX HEX ;
        escs : '\\' | '\"' | '/' | 'b' | 'f' | 'n' | 'r' | 't' ;
        upSlash : '^' | '\\' ;
HEX : < number | 'a' | 'b' | 'c' | 'd' | 'e' | 'f'  | 'A' | 'B' | 'C' | 'D' | 'E' | 'F' > ;
Number : INT optFrac optExp _ ;
        optFrac : [ FRAC ] ;
        optExp : [ EXP ] ;
INT : neg ints ;
        ints : (notZero optNums | '0' ) ; 
        neg : [ '-' ] ;
FRAC : '.' numbers ;
EXP : es plusMinus numbers ;
        notZero : not '0' number ;
        optNums : { numbers } ;
        numbers : < numbers > ;
        plusMinus : [ '+' | '-' ] ;
        es : 'e' | 'E' ;
TRUE : "true" _ ;
FALSE : "false" _ ;
NUL : "null" _ ;
COMMA : ',' _ ;
COLON : ':' _ ;
LBRACE : '{' _ ;
RBRACE : '}' _ ;
LBRACKET : '[' _ ;
RBRACKET : ']' _ ;

_ : { esChar | blockComment | comment } ;
esCharSpace : < ' ' | esChar > ;
esChar : '\t' | newLine ; 
blockComment : "*/" / "/*" blockComment ;
comment : newLine / "//" comment ; 
newLine : '\r' | '\n' ;

```
