/*
 [The "BSD licence"]
 Copyright (c) 2013 Terence Parr, Sam Harwell
 All rights reserved.

 Redistribution and use in source and binary forms, with or without
 modification, are permitted provided that the following conditions
 are met:
 1. Redistributions of source code must retain the above copyright
    notice, this list of conditions and the following disclaimer.
 2. Redistributions in binary form must reproduce the above copyright
    notice, this list of conditions and the following disclaimer in the
    documentation and/or other materials provided with the distribution.
 3. The name of the author may not be used to endorse or promote products
    derived from this software without specific prior written permission.

 THIS SOFTWARE IS PROVIDED BY THE AUTHOR ``AS IS'' AND ANY EXPRESS OR
 IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES
 OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED.
 IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR ANY DIRECT, INDIRECT,
 INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT
 NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
 DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
 THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
 (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF
 THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
*/

/** A Java 1.7 grammar for ANTLR v4 derived from ANTLR v3 Java grammar.
 *  Uses ANTLR v4's left-recursive expression notation.
 *  It parses ECJ, Netbeans, JDK etc...
 *
 *  Sam Harwell cleaned this up significantly and updated to 1.7!
 *
 *  You can test with
 *
 *  $ antlr4 Java.g4
 *  $ javac *.java
 *  $ grun Java compilationUnit *.java
 */
parser grammar LangJavaParser;

// starting point for parsing a java file
compilationUnit
    :   packageDeclaration? importDeclaration* typeDeclaration* EOF
    ;

packageDeclaration
    :   annotation* 'package' qualifiedName ';'
    ;

importDeclaration
    :   'import' 'static'? qualifiedName importAll? ';'
    ;
importAll: '.' '*';

typeDeclaration
    :   classOrInterfaceModifier* classDeclaration
    |   classOrInterfaceModifier* enumDeclaration
    |   classOrInterfaceModifier* interfaceDeclaration
    |   classOrInterfaceModifier* annotationTypeDeclaration
    |   ';'
    ;

modifier
    :   classOrInterfaceModifier
    |   (   'native'
        |   'synchronized'
        |   'transient'
        |   'volatile'
        )
    |   valEmbedModifiers
    ;

classOrInterfaceModifier
    :   annotation       // class or interface
    |   (   'public'     // class or interface
        |   'protected'  // class or interface
        |   'private'    // class or interface
        |   'static'     // class or interface
        |   'abstract'   // class or interface
        |   'final'      // class only -- does not apply to interfaces
        |   'strictfp'   // class or interface
        )
    ;

variableModifier
    :   'final'
    |   annotation
    ;

classDeclaration
    :   'class' javaIdentifier typeParameters? ext? imp? classBody
    ;
ext: 'extends' type;
imp: 'implements' typeList;

typeParameters
    :   '<' typeParameterList '>'
    ;

typeParameterList
    :   typeParameter
    |   typeParameterList ',' typeParameterList
    ;

typeParameter
    :   javaIdentifier typeParameterBound?
    ;
typeParameterBound: 'extends' typeBound;

typeBound
    :   type
    |   type '&' typeBound
    ;

enumDeclaration
    :   ENUM javaIdentifier imp?
        '{' enumConstants? ','? enumBodyDeclarations? '}'
    ;

enumConstants
    :   enumConstant
    |   enumConstants ',' enumConstant
    ;

enumConstant
    :   annotation* javaIdentifier arguments? classBody?
    ;

enumBodyDeclarations
    :   ';' classBodyDeclaration*
    ;

interfaceDeclaration
    :   'interface' javaIdentifier typeParameters? intExt? interfaceBody
    ;
intExt: 'extends' typeList;

typeList
    :   type
    |   type ',' typeList
    ;

classBody
    :   '{' classBodyDeclaration* '}'
    ;

interfaceBody
    :   '{' interfaceBodyDeclaration* '}'
    ;

classBodyDeclaration
    :   ';'
    |   'static'? block
    |   valEmbedContract? modifier* memberDeclaration
    |   valEmbedDeclarationBlock
    |   {specLevel>0}? valDeclaration
    ;

memberDeclaration
    :   methodDeclaration
    |   genericMethodDeclaration
    |   fieldDeclaration
    |   constructorDeclaration
    |   genericConstructorDeclaration
    |   interfaceDeclaration
    |   annotationTypeDeclaration
    |   classDeclaration
    |   enumDeclaration
    ;

/* We use rule this even for void methods which cannot have [] after parameters.
   This simplifies grammar and we can consider void to be a type, which
   renders the [] matching as a context-sensitive issue or a semantic check
   for invalid return type after parsing.
 */
methodDeclaration
    :   typeOrVoid javaIdentifier formalParameters dims? throwy? methodBodyOrEmpty
    ;

throwy
    :   'throws' qualifiedNameList
    ;

genericMethodDeclaration
    :   typeParameters methodDeclaration
    ;

constructorDeclaration
    :   javaIdentifier formalParameters throwy?
        constructorBody
    ;

genericConstructorDeclaration
    :   typeParameters constructorDeclaration
    ;

fieldDeclaration
    :   type variableDeclarators ';'
    ;

interfaceBodyDeclaration
    :   modifier* interfaceMemberDeclaration
    |   valEmbedDeclarationBlock
    |   ';'
    ;

interfaceMemberDeclaration
    :   constDeclaration
    |   interfaceMethodDeclaration
    |   genericInterfaceMethodDeclaration
    |   interfaceDeclaration
    |   annotationTypeDeclaration
    |   classDeclaration
    |   enumDeclaration
    ;

constDeclaration
    :   type constantDeclaratorList ';'
    ;

constantDeclaratorList
    :   constantDeclarator
    |   constantDeclarator ',' constantDeclaratorList
    ;

constantDeclarator
    :   javaIdentifier dims? '=' variableInitializer
    ;

// see matching of [] comment in methodDeclaratorRest
interfaceMethodDeclaration
    :   typeOrVoid javaIdentifier formalParameters dims? throwy? ';'
    ;

genericInterfaceMethodDeclaration
    :   typeParameters interfaceMethodDeclaration
    ;

variableDeclarators
    :   variableDeclarator
    |   variableDeclarator ',' variableDeclarators
    ;

variableDeclarator
    :   variableDeclaratorId variableDeclaratorInit?
    ;
variableDeclaratorInit: '=' variableInitializer;

variableDeclaratorId
    :   javaIdentifier dims?
    ;

variableInitializer
    :   arrayInitializer
    |   expression
    ;

arrayInitializer
    :   '{' '}'
    |   '{' variableInitializerList ','? '}'
    ;

variableInitializerList
    :   variableInitializer
    |   variableInitializer ',' variableInitializerList
    ;

enumConstantName
    :   javaIdentifier
    ;

type
    // The specification types must go first, to prevent something like "Class not found: resource"
    :   {specLevel>0}? valType
    |   classOrInterfaceType dims?
    |   primitiveType dims?
    ;

typeOrVoid
    : 'void'
    | type
    ;

dims: dim+;
dim: '[' ']';

classOrInterfaceType
    :   javaIdentifier typeArguments?
    |   classOrInterfaceType '.' javaIdentifier typeArguments?
    ;

primitiveType
    :(  'boolean'
    |   'char'
    |   'byte'
    |   'short'
    |   'int'
    |   'long'
    |   'float'
    |   'double'
    );

typeArguments
    :   '<' typeArgumentList '>'
    ;

typeArgumentList
    :   typeArgument
    |   typeArgument ',' typeArgumentList
    ;

typeArgument
    :   type
    |   '?' boundType?
    ;

boundType : ('extends' | 'super') type ;

qualifiedNameList
    :   qualifiedName
    |   qualifiedName ',' qualifiedNameList
    ;

formalParameters
    :   '(' formalParameterList? ')'
    ;

formalParameterList
    :   varargsFormalParameter
    |   initFormalParameterList
    |   initFormalParameterList ',' varargsFormalParameter
    ;

initFormalParameterList
    :   formalParameter
    |   formalParameter ',' initFormalParameterList
    ;

formalParameter
    :   variableModifier* type variableDeclaratorId
    ;

varargsFormalParameter
    :   variableModifier* type '...' variableDeclaratorId
    ;

methodBody
    :   block
    ;

methodBodyOrEmpty
    :   ';'
    |   methodBody
    ;

constructorBody
    :   block
    ;

qualifiedName
    :   javaIdentifier
    |   javaIdentifier '.' qualifiedName
    ;

literal
    :   IntegerLiteral
    |   FloatingPointLiteral
    |   CharacterLiteral
    |   StringLiteral
    |   (True|False)
    |   'null'
    ;

// ANNOTATIONS

annotation
    :   '@' annotationName annotationArgs?
    ;

annotationArgs :   '(' annotationArgsElems? ')' ;

annotationArgsElems
    : elementValuePairs
    | elementValue
    ;

annotationName : qualifiedName ;

elementValuePairs
    :   elementValuePair
    |   elementValuePairs ',' elementValuePair
    ;

elementValuePair
    :   javaIdentifier '=' elementValue
    ;

elementValue
    :   expression
    |   annotation
    |   elementValueArrayInitializer
    ;

elementValueArrayInitializer
    :   '{' elementValues? ','? '}'
    ;

elementValues
    :   elementValue
    |   elementValues ',' elementValue
    ;

annotationTypeDeclaration
    :   '@' 'interface' javaIdentifier annotationTypeBody
    ;

annotationTypeBody
    :   '{' (annotationTypeElementDeclaration)* '}'
    ;

annotationTypeElementDeclaration
    :   modifier* annotationTypeElementRest
    |   ';' // this is not allowed by the grammar, but apparently allowed by the actual compiler
    ;

annotationTypeElementRest
    :   type annotationMethodOrConstantRest ';'
    |   classDeclaration ';'?
    |   interfaceDeclaration ';'?
    |   enumDeclaration ';'?
    |   annotationTypeDeclaration ';'?
    ;

annotationMethodOrConstantRest
    :   annotationMethodRest
    |   annotationConstantRest
    ;

annotationMethodRest
    :   javaIdentifier '(' ')' defaultValue?
    ;

annotationConstantRest
    :   variableDeclarators
    ;

defaultValue
    :   'default' elementValue
    ;

// STATEMENTS / BLOCKS

block
    :   '{' blockStatement* '}'
    ;

blockStatement
    :   localVariableDeclarationStatement
    |   statement
    |   typeDeclaration
    |   valEmbedStatementBlock
    ;

localVariableDeclarationStatement
    :    localVariableDeclaration ';'
    ;

localVariableDeclaration
    :   variableModifier* type variableDeclarators
    ;

statement
    :   block
    |   ASSERT expression assertMessage? ';'
    |   'if' parExpression statement elseBlock?
    |   valEmbedContract? 'for' '(' forControl ')' valEmbedContract? statement
    |   valEmbedContract? 'while' parExpression valEmbedContract? statement
    |   'do' statement 'while' parExpression ';'
    |   'try' block catchClause+ finallyBlock?
    |   'try' block finallyBlock
    |   'try' resourceSpecification block catchClause* finallyBlock?
    |   'switch' parExpression '{' switchBlockStatementGroup* switchLabel* '}'
    |   'synchronized' parExpression block
    |   'return' expression? ';'
    |   'throw' expression ';'
    |   'break' javaIdentifier? ';'
    |   'continue' javaIdentifier? ';'
    |   ';'
    |   statementExpression ';'
    |   valEmbedContract? javaIdentifier ':' statement
    |   {specLevel>0}? valStatement
    ;

assertMessage: ':' expression;
elseBlock: 'else' statement;

catchClause
    :   'catch' '(' variableModifier* catchType javaIdentifier ')' block
    ;

catchType
    :   qualifiedName
    |   qualifiedName '|' catchType
    ;

finallyBlock
    :   'finally' block
    ;

resourceSpecification
    :   '(' resources ';'? ')'
    ;

resources
    :   resource
    |   resource ';' resources
    ;

resource
    :   variableModifier* classOrInterfaceType variableDeclaratorId '=' expression
    ;

/** Matches cases then statements, both of which are mandatory.
 *  To handle empty cases at the end, we add switchLabel* to statement.
 */
switchBlockStatementGroup
    :   switchLabel+ blockStatement+
    ;

switchLabel
    :   'case' constantExpression ':'
    |   'case' enumConstantName ':'
    |   'default' ':'
    ;

forControl
    :   enhancedForControl
    |   forInit? ';' expression? ';' forUpdate?
    ;

forInit
    :   localVariableDeclaration
    |   expressionList
    ;

enhancedForControl
    :   variableModifier* type variableDeclaratorId ':' expression
    ;

forUpdate
    :   expressionList
    ;

// EXPRESSIONS

parExpression
    :   '(' expression ')'
    ;

expressionList
    :   expression
    |   expression ',' expressionList
    ;

statementExpression
    :   expression
    ;

constantExpression
    :   expression
    ;

expression
    :   {specLevel>0}? valPrimary
    |   primary
    |   expression '.' javaIdentifier
    |   expression '.' 'this'
    |   expression '.' 'new' nonWildcardTypeArguments? innerCreator
    |   expression '.' 'super' superSuffix
    |   expression '.' explicitGenericInvocation
    |   expression '[' expression ']'
    |   expression '->' javaIdentifier arguments
    |   expression '.' javaIdentifier predicateEntryType? arguments valEmbedWithThen?
    |   'new' creator valEmbedWithThen?
    |   '(' type ')' expression
    |   expression ('++' | '--')
    |   ('+'|'-'|'++'|'--') expression
    |   ('~'|'!') expression
    |   expression mulOp expression
    |   expression ('+'|'-') expression
    |   expression shiftOp expression
    |   expression ('<=' | '>=' | '>' | '<') expression
    |   expression 'instanceof' type
    |   expression ('==' | '!=') expression
    |   expression '&' expression
    |   expression '^' expression
    |   expression '|' expression
    |   expression andOp expression
    |   expression '||' expression
    |   expression impOp  expression
    |   expression '?' expression ':' expression
    |   <assoc=right> expression assignOp expression
    ;
predicateEntryType: '@' javaIdentifier; // TODO: Find correct class type
mulOp
    : ('*'|'/'|'%')
    | {specLevel>0}? valMulOp
    ;
andOp
    : ('&&')
    | {specLevel>0}? valAndOp
    ;
impOp
    : {specLevel>0}? valImpOp
    ;
shiftOp
    :   '<' '<'
    |   '>' '>' '>'
    |   '>' '>'
    ;
assignOp
    :   '='
    |   '+='
    |   '-='
    |   '*='
    |   '/='
    |   '&='
    |   '|='
    |   '^='
    |   '>>='
    |   '>>>='
    |   '<<='
    |   '%='
    ;

primary
    :   '(' expression ')'
    |   'this'
    |   'super'
    |   literal
    |   javaIdentifier callTail?
    |   type '.' 'class'
    |   'void' '.' 'class'
    |   nonWildcardTypeArguments constructorCall
	;

callTail : javaIdentifier predicateEntryType? arguments valEmbedWithThen?;

constructorCall
    :   explicitGenericInvocationSuffix
    |   'this' arguments
    ;

creator
    :   nonWildcardTypeArguments createdName classCreatorRest
    |   createdName creatorRest
    ;

creatorRest: arrayCreatorRest | classCreatorRest;

createdName
    :   classTypeDiamondList
    |   primitiveType
    ;

classTypeDiamondList
    :   javaIdentifier typeArgumentsOrDiamond?
    |   javaIdentifier typeArgumentsOrDiamond? '.' classTypeDiamondList
    ;

innerCreator
    :   javaIdentifier nonWildcardTypeArgumentsOrDiamond? classCreatorRest
    ;

arrayCreatorRest
    :   dims arrayInitializer
    |   specifiedDims dims?
    ;

specifiedDims
    :   specifiedDim
    |   specifiedDim specifiedDims
    ;

specifiedDim: '[' expression ']';

classCreatorRest
    :   arguments classBody?
    ;

explicitGenericInvocation
    :   nonWildcardTypeArguments explicitGenericInvocationSuffix
    ;

nonWildcardTypeArguments
    :   '<' typeList '>'
    ;

typeArgumentsOrDiamond
    :   '<' '>'
    |   typeArguments
    ;

nonWildcardTypeArgumentsOrDiamond
    :   '<' '>'
    |   nonWildcardTypeArguments
    ;

superSuffix
    :   arguments
    |   '.' javaIdentifier arguments?
    ;

explicitGenericInvocationSuffix
    :   'super' superSuffix
    |   javaIdentifier predicateEntryType? arguments
    ;

arguments
    :   '(' expressionList? ')'
    ;

javaIdentifier
    : {specLevel>0}? valReserved
    | Identifier
    | valReserved // allow reserved identifiers outside specification
    ;