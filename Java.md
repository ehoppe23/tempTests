// // Java grammar
Modification of `Java` grammar from [Egg](https://github.com/bruceiv/egg/blob/deriv/grammars/Java-u.egg) to test Java grammar.

```
package "Java"

//-------------------------------------------------------------
//  @ Author : Roman R Redziejowski Copyright (C) 2006
//  (http://home.swipnet.se/redz/roman).
//
//  This is free software; you can redistribute and/or modify
//  it under the terms of the GNU Library General Public License
//  as published by the Free Software Foundation; either version 2
//  of the License or (at your option) any later version.
//
//  This file is distributed in the hope that it will be useful,
//  but WITHOUT ANY WARRANTY; without even the implied warranty of
//  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
//
//  For more details, see the GNU Library General Public License
//  at http://www.fsf.org/copyleft/gpl.html.
//
//  @ Modified to PegGLL Grammar : Brynn Harrington and Emily Hoppe
//  Date : June 10, 2021 

//-------------------------------------------------------------
//  Compilation Unit
//-------------------------------------------------------------

CompilationUnit : _ optPackDecl repImpDecl0 repSemiAltModifs0 ;
      optPackDecl : [ PackageDeclaration ] ;
      repImpDecl0 : { ImportDeclaration } ; 
      repSemiAltModifs0 : { SEMI | repModif0 declarAlts } ;
      declarAlts : ClassDeclaration  | InterfaceDeclaration ;

PackageDeclaration : PACKAGE QualifiedIdentifier SEMI ;

ImportDeclaration : IMPORT optStatic QualifiedIdentifier optDotStar SEMI ;
   optDotStar : [ DOT STAR ] ;

//-------------------------------------------------------------
//  Class Declaration
//-------------------------------------------------------------

ClassDeclaration : CLASS Identifier optExtClassType optImpleClaTLis ClassBody ;
      optExtClassType : [ EXTENDS ClassType ] ;
      optImpleClaTLis : [ IMPLEMENTS ClassTypeList ] ;
      

ClassBody : LWING repClassBodyDecl0 RWING
   repClassBodyDecl0 : { ClassBodyDeclaration } ;

ClassBodyDeclaration : SEMI
      | optStatic Block  // Static or Instance Initializer
      | repModif0 MemberDecl   // ClassMemberDeclaration    
      optStatic : [ STATIC ] ; 

MemberDecl : Type Identifier FormalParameters  dims0 optThrowClassTypeList altSemiBlock // Method
    | VOID Identifier FormalParameters  optThrowClassTypeList altSemiBlock   // Void method
    | Identifier FormalParameters  optThrowClassTypeList Block  // Constructor
    | InterfaceDeclaration  // Interface
    | ClassDeclaration      // Class
    | Type VariableDeclarator repComVarDec0 ; // Field
    altSemiBlock : SEMI | Block ;

//-------------------------------------------------------------
//  Interface Declaration
//-------------------------------------------------------------

InterfaceDeclaration : INTERFACE Identifier optExtendsClaLis InterfaceBody ;
        optExtendsClaLis : [ EXTENDS ClassTypeList ] ;

InterfaceBody : LWING repInterBodDecl0 RWING ;
        repInterBodDecl0 : { InterfaceBodyDeclaration } ;

InterfaceBodyDeclaration: repModif0 InterfaceMemberDecl | SEMI ;

InterfaceMemberDecl : InterfaceMethodOrFieldDecl
    | VOID Identifier VoidInterfaceMethodDeclaratorRest
    | InterfaceDeclaration
    | ClassDeclaration ;

InterfaceMethodOrFieldDecl : Type Identifier InterfaceMethodOrFieldRest ;

InterfaceMethodOrFieldRest : ConstantDeclaratorsRest SEMI | InterfaceMethodDeclaratorRest ;

InterfaceMethodDeclaratorRest : FormalParameters dims0 optThrowClassTypeList SEMI ;

VoidInterfaceMethodDeclaratorRest : FormalParameters optThrowClassTypeList SEMI ;
        optThrowClassTypeList : [ THROWS ClassTypeList ] ;

ConstantDeclaratorsRest : ConstantDeclaratorRest repComConstDecl0 ;
        repComConstDecl0 : { COMMA ConstantDeclarator } ;

ConstantDeclarator : Identifier ConstantDeclaratorRest ;

ConstantDeclaratorRest : dims0 EQU VariableInitializer ;
    
//-------------------------------------------------------------
//  Variable Declarations
//-------------------------------------------------------------

LocalVariableDeclarationStatement : optFinal Type VariableDeclarator repComVarDec0 SEMI ;

VariableDeclarator :  Identifier dims0 optEqVarInit ;
        optEqVarInit : [ EQU  VariableInitializer ] ;

//-------------------------------------------------------------
//  Formal Parameters
//-------------------------------------------------------------

FormalParameters : LPAR optFormParaDecl RPAR ;
        optFormParaDecl : [ FormalParameterDecls ] ;

FormalParameter : optFinal Type VariableDeclaratorId ;
 
FormalParameterDecls : optFinal Type FormalParameterDeclsRest ;

FormalParameterDeclsRest :  VariableDeclaratorId optComFormParaDecl ;
        optComFormParaDecl : [ COMMA FormalParameterDecls ] ;

VariableDeclaratorId : Identifier dims0 ;

//-------------------------------------------------------------
//  Statements
//-------------------------------------------------------------

Block : LWING repBlockStmt0 RWING ;
      repBlockStmt0 : { BlockStatement } ;

BlockStatement : LocalVariableDeclarationStatement
   | repModif0 ClassDeclaration
   | Statement ;
   repModif0 : { Modifier } ;

Statement : Block
   | ASSERT Expression optColExpr SEMI
   | IF ParExpression Statement optElseStmt
   | FOR LPAR optForInit SEMI optExpr SEMI optForUpd RPAR Statement
   | WHILE ParExpression Statement
   | DO Statement WHILE ParExpression SEMI
   | TRY Block catchBlock
   | SWITCH ParExpression LWING repSwitchBlockStatementGroup0 RWING
   | SYNCHRONIZED ParExpression Block
   | RETURN optExpr SEMI
   | THROW Expression SEMI
   | BREAK optIdent SEMI
   | CONTINUE optIdent SEMI
   | SEMI
   | StatementExpression SEMI
   | Identifier COLON Statement ; 
      optColExpr : [ COLON Expression ] ;
      optElseStmt : [ ELSE Statement ] ;
      optForInit : [ ForInit ] ;
      optForUpd : [ ForUpdate ] ;
      optExpr : [ Expression ] ;
      catchBlock : repCatch1 optFinally | Finally ;
      repCatch1 : < Catch > ;
      optFinally : [ Finally ] ;
      repSwitchBlockStatementGroup0 : { SwitchBlockStatementGroup } ;
      optIdent : [ Identifier ] ;

Catch : CATCH LPAR FormalParameter RPAR Block ;
   
Finally : FINALLY Block ;

SwitchBlockStatementGroup : SwitchLabel repBlockStmt0;
   repBlockStmt0 : { BlockStatement } ;


SwitchLabel : CASE ConstantExpression COLON | DEFAULT COLON ;

ForInit: optFinal Type VariableDeclarator repComVarDec0 ;
   | StatementExpression repComStatExpr0 ; 
   optFinal : [ FINAL ] ;
   repComVarDec0 : { COMMA VariableDeclarator } ;

ForUpdate : StatementExpression repComStatExpr0 ;
      repComStatExpr0 : { COMMA StatementExpression };

//-------------------------------------------------------------
//  Expressions
//-------------------------------------------------------------

StatementExpression : Expression ;

   // This is more generous than definition in section 14.8, 
   // which allows only specific forms of Expression.
   
ConstantExpression : Expression ;

Expression : ConditionalExpression repAssOpCondExpr ;
      repAssOpCondExpr : { AssignmentOperator ConditionalExpression } ;
   
   // This definition is part of the modification 
   // in JLS Chapter 18
   // to minimize look ahead. In JLS Chapter 15.27, Expression
   // is defined as AssignmentExpression, which is effectively
   // defined as
   // (LeftHandSide AssignmentOperator)* ConditionalExpression.
   // The above is obtained by allowing 
   // ANY ConditionalExpression as LeftHandSide, 
   // which results in accepting statements like 5 : a.
   

AssignmentOperator : EQU
   | PLUS_EQU
   | MINUS_EQU
   | STAR_EQU
   | DIV_EQU
   | AND_EQU
   | OR_EQU
   | HAT_EQU
   | MOD_EQU
   | SL_EQU
   | SR_EQU
   | BSR_EQU ;

ConditionalExpression : ConditionalOrExpression repQueryExpColonCondExp0
   repQueryExpColonCondExp0 : { QUERY Expression COLON ConditionalOrExpression } ;

ConditionalOrExpression: ConditionalAndExpression repOrConAndExpr0 ;
      repOrConAndExpr0 : { OR_OR ConditionalAndExpression } ;

ConditionalAndExpression : InclusiveOrExpression repAndInclOrExpr0 ;
      repAndInclOrExpr0 : { AND_AND InclusiveOrExpression } ;

InclusiveOrExpression : ExclusiveOrExpression repORXOR0 ;
   repORXOR0 : { ORXOR } ;
      ORXOR : OR ExclusiveOrExpression ;

ExclusiveOrExpression : AndExpression repHatAndExpr0 ;
      repHatAndExpr0 : { HAT AndExpression };

AndExpression : EqualityExpression repAndEqExpr0 ;
      repAndEqExpr0 : { AND EqualityExpression } ;

EqualityExpression : RelationalExpression repEqRelExpr0 ;
   repEqRelExpr0 : {eqAlts RelationalExpression } ;
   eqAlts : EQUAL |  NOT_EQUAL ;

RelationalExpression: ShiftExpression repEqualShiftInstance0 ;
   repEqualShiftInstance0 : { equalShiftInstance } ;
   equalShiftInstance : equalShift | INSTANCEOF ReferenceType ;
   equalShift : equalCheck ShiftExpression ;
   equalCheck : LE | GE | LT | GT ;

ShiftExpression : AdditiveExpression altsAddExpr0 ;
      carrotAlts : SL | SR | BSR ;
      altsAddExpr0 : { carrotAlts AdditiveExpression };


AdditiveExpression : MultiplicativeExpression repAddAltsMultExpr0 ;
   repAddAltsMultExpr0 : { addAlts MultiplicativeExpression } ;
   addAlts : PLUS | MINUS ;

MultiplicativeExpression : UnaryExpression repStarDivMod_UExpress0 ;
   repStarDivMod_UExpress0 : { starDivMod UnaryExpression } ;
   starDivMod : STAR | DIV | MOD ;

UnaryExpression : PrefixOp UnaryExpression
   | LPAR Type RPAR UnaryExpression
   | Primary selectors0 repPostfixOp0 ;
   selectors0 : { Selector } ;
   repPostfixOp0 : { PostfixOp } ;

Primary: ParExpression
   | THIS optArgs
   | SUPER SuperSuffix
   | Literal
   | NEW Creator
   | QualifiedIdentifier optIdentSuff
   | BasicType dims0 DOT CLASS
   | VOID DOT CLASS ;
   optIdentSuff : [ IdentifierSuffix ] ;

IdentifierSuffix : LBRK rbrkAlts | Arguments | DOT otherDotsAlts ;
      rbrkAlts : RBRK dims0 DOT CLASS | Expression RBRK ; 
      otherDotsAlts : CLASS | THIS  | SUPER Arguments | NEW InnerCreator ;
      


PrefixOp : INC | DEC | BANG | TILDA | PLUS | MINUS ;
PostfixOp : INC | DEC ;
Selector : DOT Identifier optArgs
   | DOT THIS
   | DOT SUPER SuperSuffix
   | DOT NEW InnerCreator
   | DimExpr ;


SuperSuffix : Arguments | DOT Identifier optArgs ;
      optArgs : [ Arguments ] ;

BasicType : basicTypes not LetterOrDigit _ ;
   basicTypes :  "byte"
   | "short"
   | "char"
   | "int"
   | "long"
   | "float"
   | "double"
   | "boolean" ;

Arguments : LPAR optExprs RPAR ;
      optExprs : [ Expression comExp0 ] ;
      comExp0 : { COMMA Expression };

Creator : CreatedName ClassCreatorRest | typeAlts ArrayCreatorRest ;
CreatedName : Identifier dotIdent0 ;
      dotIdent0 : { DOT Identifier } ;
InnerCreator : Identifier ClassCreatorRest ;
ArrayCreatorRest : LBRK arrInitRest;
   arrInitRest : RBRK dims0 ArrayInitializer | Expression RBRK repDimExp0 dims0 ;
   repDimExp0 : { DimExpr } ;

   // This is more generous than JLS 15.10. According to that 
   // definition, BasicType must be followed by at least one 
   // DimExpr or by ArrayInitializer.
   
ClassCreatorRest :  Arguments optClassBody ;
   optClassBody : [ ClassBody ] ;

ArrayInitializer : LWING optVarInits  RWING ;
   optVarInits : [ VariableInitializer repComVarInits0 optComma ] ;
   repComVarInits0 : { COMMA  VariableInitializer } ;
   optComma : [ COMMA ] ;


VariableInitializer : ArrayInitializer | Expression ;

ParExpression : LPAR Expression RPAR ;

QualifiedIdentifier  : Identifier dotIdenRep0;
        dotIdenRep0: { DOT Identifier } ;

Dim : LBRK RBRK ;
DimExpr : LBRK Expression RBRK ; 

//-------------------------------------------------------------
//  Types and Modifiers
//-------------------------------------------------------------

Type : typeAlts dims0 ; 
      typeAlts : BasicType | ClassType ;

ReferenceType : BasicType dims1 | ClassType dims0 ;
      dims0 : { Dim } ;
      dims1 : < Dim > ;

ClassType : Identifier dotIdents0 ;
      dotIdents0 : { DOT Indentifier} ;

ClassTypeList : ClassType comCType0 ;
      comCType0 : { COMMA ClassType } ;

Modifier : modifs not LetterOrDigit _ ;
   modifs : "public"
   | "protected"
   | "private"
   | "static"
   | "abstract"
   | "final"
   | "native"
   | "synchronized"
   | "transient"
   | "volatile"
   | "strictfp" ;
   
   
   // This common definition of Modifier is part of the 
   // modification in JLS Chapter 18 to minimize look ahead. 
   // The main body of JLS has different lists of modifiers 
   // for different language elements.
//////////////////////////////////////////////////////////////////////////////////////////////////////////////// come back to edit more
//-------------------------------------------------------------
//  Lexical Structure
//-------------------------------------------------------------
//-------------------------------------------------------------
//  JLS 3.6-7  Spacing
//-------------------------------------------------------------
_ : { EscCharSpace       // match a whitespace character
      | BlockComment 
      | Comment 
      } ;
            EscCharSpace :    // match escape character or space at least once
                  < ' ' | EscChar > ;
            EscChar :         // match tab or new line 
                  '\t' | NewLine ; 
            BlockComment :    // match /* -> */ (block comment)
                  "*/" / "/*" BlockComment ;
            Comment :         // match // -> \n (comment)
                  NewLine / "//" Comment ; 
            NewLine :         // match new line escape character 
                  '\r' | '\n' ;
//-------------------------------------------------------------
//  JLS 3.8  Identifiers
//-------------------------------------------------------------
Identifier  : 
      not Keyword Letter repLOD0 _ ;   
            repLOD0 :         // match letter or digit zero or more times
                  { LetterOrDigit } ;
   
Letter :                      // optional match letter 
      letter | _ ;

LetterOrDigit :               // optional match letter or digit 
      Letter 
      | Digit 
      | _ ;

// These are traditional definitions of letters and RepDigit0x.
// JLS defines letters and RepDigit0x as Unicode characters 
// recognized as such by special Java procedures, which is 
// difficult to express in terms of Parsing Expressions.
//-------------------------------------------------------------
//  JLS 3.9  Keywords
//-------------------------------------------------------------
Keyword : Words not LetterOrDigit  ;
Words : 
        "abstract" 
      | "assert"   
      | "boolean"  
      | "break"    
      | "byte"     
      | "case"     
      | "catch"    
      | "char"     
      | "class"    
      | "continue" 
      | "default"  
      | "double"   
      | "do"       
      | "else"     
      | "enum"     
      | "extends"  
      | "false"    
      | "finally"  
      | "final"    
      | "float"    
      | "for"      
      | "if"       
      | "implements"
      | "import"   
      | "interface"
      | "int"      
      | "instanceof"
      | "long"     
      | "native"   
      | "new"      
      | "null"     
      | "package"  
      | "private"  
      | "protected"
      | "public"   
      | "return"   
      | "short"    
      | "static"   
      | "strictfp" 
      | "super"    
      | "switch"   
      | "synchronized"
      | "this"
      | "throws"   
      | "throw"    
      | "transient"
      | "true"     
      | "try"      
      | "void"     
      | "volatile" 
      | "while" ;

ASSERT       : "assert"       not LetterOrDigit _ ; 
BREAK        : "break"        not LetterOrDigit _ ;
CASE         : "case"         not LetterOrDigit _ ;
CATCH        : "catch"        not LetterOrDigit _ ;
CLASS        : "class"        not LetterOrDigit _ ;
CONTINUE     : "continue"     not LetterOrDigit _ ;
DEFAULT      : "default"      not LetterOrDigit _ ;
DO           : "do"           not LetterOrDigit _ ;
ELSE         : "else"         not LetterOrDigit _ ;
ENUM         : "enum"         not LetterOrDigit _ ;
EXTENDS      : "extends"      not LetterOrDigit _ ;
FINALLY      : "finally"      not LetterOrDigit _ ; 
FINAL        : "final"        not LetterOrDigit _ ;
FOR          : "for"          not LetterOrDigit _ ;
IF           : "if"           not LetterOrDigit _ ; 
IMPLEMENTS   : "implements"   not LetterOrDigit _ ; 
IMPORT       : "import"       not LetterOrDigit _ ;
INTERFACE    : "interface"    not LetterOrDigit _ ;
INSTANCEOF   : "instanceof"   not LetterOrDigit _ ;
NEW          : "new"          not LetterOrDigit _ ;
PACKAGE      : "package"      not LetterOrDigit _ ; 
RETURN       : "return"       not LetterOrDigit _ ;
STATIC       : "static"       not LetterOrDigit _ ; 
SUPER        : "super"        not LetterOrDigit _ ;
SWITCH       : "switch"       not LetterOrDigit _ ;
SYNCHRONIZED : "synchronized" not LetterOrDigit _ ; 
THIS         : "this"         not LetterOrDigit _ ;
THROWS       : "throws"       not LetterOrDigit _ ;
THROW        : "throw"        not LetterOrDigit _ ;
TRY          : "try"          not LetterOrDigit _ ;
VOID         : "void"         not LetterOrDigit _ ;
WHILE        : "while"        not LetterOrDigit _ ; 

//-------------------------------------------------------------
//  JLS 3.10 Literals
//-------------------------------------------------------------
Literal : 
      LitAlts _ ;
            LitAlts :               // match types of literals
                  FloatLiteral
                  | IntegerLiteral 
                  | CharLiteral
                  | StringLiteral
                  | "true"  not LetterOrDigit
                  | "false" not LetterOrDigit
                  | "null"  not LetterOrDigit ;
//-------------------------------------------------------------
//  JLS 3.10.1 Character and String Literals
//-------------------------------------------------------------
CharLiteral : 
      '\'' EscSlash '\'' ;
            EscSlash :        // match escape characters
                  ( Escape 
                  | EscUp ) ;
            EscUp : 
                  '^' 
                  | '\'' 
                  | '\\' ;

StringLiteral : 
      '\"' StrClose ;
            StrClose :              // match closing double quote using " -> " idea
                  '\"' 
                  / OptEsc StrClose ;
            OptEsc :                // optional match on escape characters
                  ( Escape 
                  | [^\\] ) ;

Escape : 
      "\\" Escs ;
            Escs:             // match different types of escape characters
                  ( EsChars 
                  | OctalEscape 
                  | UnicodeEscape) ;
            EsChars:          // match escape characters
                  '\\' 
                  | '\"' 
                  | '\'' 
                  | 'b' 
                  | 'f' 
                  | 'n' 
                  | 'r' 
                  | 't' ;
   
UnicodeEscape : 
      "u" HexDigit HexDigit HexDigit HexDigit ;
   // NOTE: Unicode escape is not defined in JLS syntax because 
   // unicode characters are processed very early (see 3.10.4).
//-------------------------------------------------------------
//  JLS 3.10.2 Numeric Literals
//-------------------------------------------------------------
FloatLiteral :                // match a hexadecimal or decimal float 
      HexFloat 
      | DecimalFloat ;

IntegerLiteral : 
      NumeralAlts OneL ;
            NumeralAlts :     // OctalNumeral may prefix HexNumeral and 
                              // DecimalNumeral may prefix OctalNumeral
                  HexNumeral 
                  | OctalNumeral  
                  | DecimalNumeral ;
            OneL :            // optional match for '1' or 'L' literal 
                  [ '1' 
                  | 'L' 
                  ] ;

DecimalFloat : 
      RepDigits1x "." RepDigit0x OptExpon OptfF_dD 
      | "." RepDigits1x Exponent 
      | RepDigits1x Exponent OptfF_dD
      | RepDigits1x OptExpon fF_dD ;
            OptExpon :        // optional match for an exponent 
                  [ Exponent ] ;
            
            
//-------------------------------------------------------------
//  JLS 3.10.2 Different-Number Base Literals
//-------------------------------------------------------------
HexFloat : 
      HexSignificand BinaryExponent OptfFdD ;
            OptfF_dD :        // optional match for 'f' , 'F' , 'd', or 'D' literals
                  [ fF_dD ] ; 
            fF_dD :           // match 'f' , 'F' , 'd', or 'D' literals
                  'f' 
                  | 'F' 
                  | 'd' 
                  | 'D' ;

HexSignificand : 
      HexNumeral OptDot 
      | '0' xX RepHexDigits0x "." RepHexDigits1x ;
            OptDot :          // optional match for '.' literal 
                  [ '.' ] ; 

HexNumeral : 
      '0' xX RepHexDigits1x ; 
            xX :              // match 'x' or 'X' literal
                  'x' 
                  | 'X' ;
            RepHexDigits0x :  // match hexadecimal digit(s) zero or more times
                  { HexDigit } ;  
            RepHexDigits1x :  // match hexadecimal digit(s) one or more times
                  < HexDigit > ;  

HexDigit :                    // match hexadecimal digit(s) (number followed by aA-fF)
      < number 
      | 'a' 
      | 'b' 
      | 'c' 
      | 'd' 
      | 'e' 
      | 'f'  
      | 'A' 
      | 'B' 
      | 'C' 
      | 'D' 
      | 'E' 
      | 'F' 
      > ;

OctalNumeral : 
      "0" RepZero-Seven1x ; 
            RepZero-Seven1x :  // match digit(s) in range of zero-seven at least once
                  < Zero-Seven > ;

OctalEscape : 
      Zero-Three Zero-Seven Zero-Seven 
      / Zero-Seven Zero-Seven 
      / Zero-Seven ;
            Zero-Seven : // match a digit in range of zero-seven
                  Zero-Three 
                  | '4' 
                  | '5' 
                  | '6' 
                  | '7' ;
            Zero-Three : // match a digit in range of zero-three
                  '0' 
                  | '1' 
                  | '2' 
                  | '3' ;
//-------------------------------------------------------------
//  JLS 3.10.3 Exponent Literals
//-------------------------------------------------------------
Exponent : 
      eE OptPSM RepDigit0x ;
            eE :              // match 'e' or 'E' literal
                  'e' 
                  | 'E' ;
            RepDigit0x :      // match digit(s) zero or more times
                  { Digit } ; 

BinaryExponent :
      pP OptPSM RepDigits1x ;
            pP :              // match 'p' or 'P' literal
                  'p'             
                  | 'P' ;
            OptPSM :          // optional match plus, slash, or minus literal   
                  [ PSM ] ;       
            PSM :             // match plus, slash, or minus literal
                  '+'             
                  | '\' 
                  | '-' ;
            RepDigits1x :     // match digit(s) at least once
                  < Digit > ;     

Digit : number ;        
//-------------------------------------------------------------
//  JLS 3.11-12  Separators, Operators
//-------------------------------------------------------------

AT             :  '@'       _ ;
AND            :  '&'![=&]  _ ;
AND_AND        :  "&&"      _ ;
AND_EQU        :  "&="      _ ;
BANG           :  '!' !'='  _ ;
BSR            :  ">>>"!'=' _ ;
BSR_EQU        :  ">>>="    _ ;
COLON          :  ':'       _ ;
COMMA          :  ','       _ ;
DEC            :  "--"      _ ;
DIV            :  '/' !'='  _ ;
DIV_EQU        :  "/="      _ ;
DOT            :  '.'       _ ;
EQU            :  '=' !'='  _ ;
EQUAL          :  "=="      _ ;
GE             :  ">="      _ ;
GT             :  '>'![=>]  _ ;
HAT            :  '^'!'='   _ ;
HAT_EQU        :  "^="      _ ;
INC            :  "++"      _ ;
LBRK           :  '['       _ ;
LE             :  "<="      _ ;
LPAR           :  '('       _ ;
LPOINT         :  '<'       _ ;
LT             :  '<'![=<]  _ ;
LWING          :  '{'       _ ;
MINUS          :  '-'![=\-] _ ;
MINUS_EQU      :  "-="      _ ;
MOD            :  '%'!'='   _ ;
MOD_EQU        :  "%="      _ ;
NOT_EQUAL      :  "!="      _ ;
OR             :  '|'![=|]  _ ;
OR_EQU         :  "|="      _ ;
OR_OR          :  "||"      _ ;
PLUS           :  '+'![=+]  _ ;
PLUS_EQU       :  "+="      _ ;
QUERY          :  '?'       _ ;
RBRK           :  ']'       _ ;
RPAR           :  ')'       _ ;
RPOINT         :  '>'       _ ;
RWING          :  '}'       _ ;
SEMI           :  ';'       _ ;
SL             :  "<<"!'='  _ ;
SL_EQU         :  "<<="     _ ;
SR             :  ">>"![=>] _ ;
SR_EQU         :  ">>="     _ ;
STAR           :  '*'!'='   _ ;
STAR_EQU       :  "*="      _ ;
TILDA          :  '~'       _ ;

```