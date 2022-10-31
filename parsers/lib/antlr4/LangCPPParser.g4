/*******************************************************************************
 * The MIT License (MIT)
 *
 * Copyright (c) 2015 Camilo Sanchez (Camiloasc1) 2020 Martin Mirchev (Marti2203)
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy of this software and
 * associated documentation files (the "Software"), to deal in the Software without restriction,
 * including without limitation the rights to use, copy, modify, merge, publish, distribute,
 * sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all copies or
 * substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT
 * NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM,
 * DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
 * ****************************************************************************
 */
parser grammar LangCPPParser;

/*Basic concepts*/

translationUnit: declarationseq? EOF;
/*Expressions*/

primaryExpression:
    valExpr
	| literal
	| This
	| LeftParen expression RightParen
	| idExpression
	| lambdaExpression
	;

literal:
	IntegerLiteral
	| CharacterLiteral
	| FloatingLiteral
	| StringLiteral
	| BooleanLiteral
	| PointerLiteral
	| UserDefinedLiteral;

annotatedPrimaryExpression
    : valEmbedWith? primaryExpression valEmbedThen?
    ;

idExpression: unqualifiedId | qualifiedId;

unqualifiedId:
	cppIdentifier
	| operatorFunctionId
	| conversionFunctionId
	| literalOperatorId
	| Tilde className
	| Tilde decltypeSpecifier
	| templateId;

qualifiedId: nestedNameSpecifier Template? unqualifiedId;

nestedNameSpecifier:
	theTypeName? Doublecolon
	| namespaceName? Doublecolon
	| decltypeSpecifier? Doublecolon
	| nestedNameSpecifier cppIdentifier Doublecolon
	| nestedNameSpecifier Template? simpleTemplateId Doublecolon;

lambdaExpression:
	lambdaIntroducer lambdaDeclarator? compoundStatement;

lambdaIntroducer: LeftBracket lambdaCapture? RightBracket;

lambdaCapture:
	captureList
	| captureDefault Comma captureList
	| captureDefault ;

captureDefault: And | Equal;

captureList:
    capture Ellipsis?
    | capture Comma captureList ;

capture: simpleCapture | initcapture;

simpleCapture: And? cppIdentifier | This;

initcapture: And? cppIdentifier initializer;

lambdaDeclarator:
	LeftParen parameterDeclarationClause RightParen Mutable? exceptionSpecification?
		attributeSpecifierSeq? trailingReturnType?;

postfixExpression:
	annotatedPrimaryExpression
	| postfixExpression LeftBracket expression RightBracket
	| postfixExpression LeftBracket bracedInitList RightBracket
	| postfixExpression LeftParen expressionList? RightParen valEmbedGiven? valEmbedYields?
	| simpleTypeSpecifier LeftParen expressionList? RightParen valEmbedGiven? valEmbedYields?
	| simpleTypeSpecifier bracedInitList valEmbedGiven? valEmbedYields?
	| typeNameSpecifier LeftParen expressionList? RightParen valEmbedGiven? valEmbedYields?
	| typeNameSpecifier bracedInitList valEmbedGiven? valEmbedYields?
	| postfixExpression Dot Template? idExpression
	| postfixExpression Dot pseudoDestructorName
	| postfixExpression Arrow Template? idExpression
	| postfixExpression Arrow pseudoDestructorName
	| postfixExpression PlusPlus
	| postfixExpression MinusMinus
	| postfixExpression specPostfix
	| Dynamic_cast Less theTypeId Greater LeftParen expression RightParen
	| Static_cast Less theTypeId Greater LeftParen expression RightParen
	| Reinterpret_cast Less theTypeId Greater LeftParen expression RightParen
	| Const_cast Less theTypeId Greater LeftParen expression RightParen
	| typeIdOfTheTypeId LeftParen expression RightParen
	| typeIdOfTheTypeId LeftParen theTypeId RightParen;

specPostfix
    :   {specLevel>0}? valPostfix
    ;

/*
 add a middle layer to eliminate duplicated function declarations
 */
typeIdOfTheTypeId: Typeid_;

expressionList: initializerList;

pseudoDestructorName:
	nestedNameSpecifier? theTypeName Doublecolon Tilde theTypeName
	| nestedNameSpecifier?  Tilde theTypeName
	| nestedNameSpecifier Template simpleTemplateId Doublecolon Tilde theTypeName
	| Tilde decltypeSpecifier;

unaryExpression:
	postfixExpression
	| PlusPlus unaryExpression
	| MinusMinus unaryExpression
	| unaryOperator unaryExpression
	| Sizeof unaryExpression
	| Sizeof LeftParen theTypeId RightParen
	| Sizeof Ellipsis LeftParen cppIdentifier RightParen
	| Alignof LeftParen theTypeId RightParen
	| noExceptExpression
	| newExpression
	| deleteExpression;

unaryOperator: Or | Star | And | Plus | Tilde | Minus | Not;

newExpression:
	Doublecolon? New newPlacement? newTypeId newInitializer?
	| Doublecolon? New newPlacement? LeftParen theTypeId RightParen newInitializer?;

newPlacement: LeftParen expressionList RightParen;

newTypeId: typeSpecifierSeq newDeclarator?;

newDeclarator:
	pointerOperator newDeclarator?
	| noPointerNewDeclarator;

noPointerNewDeclarator:
	LeftBracket expression RightBracket attributeSpecifierSeq?
	| noPointerNewDeclarator LeftBracket constantExpression RightBracket attributeSpecifierSeq?;

newInitializer:
	LeftParen expressionList? RightParen
	| bracedInitList;

deleteExpression:
	Doublecolon? Delete (LeftBracket RightBracket)? castExpression;

noExceptExpression: Noexcept LeftParen expression RightParen;

castExpression:
	unaryExpression
	| LeftParen theTypeId RightParen castExpression;

prependExpression
    :   castExpression prependOp prependExpression
    |   castExpression
    ;

prependOp
    :   {specLevel>0}? valPrependOp
    ;

pointerMemberExpression:
	prependExpression
	| pointerMemberExpression DotStar prependExpression
	| pointerMemberExpression ArrowStar prependExpression;

multiplicativeExpression:
	pointerMemberExpression ';'
	| multiplicativeExpression multiplicativeOp pointerMemberExpression;

multiplicativeOp
   : Star
   | Div
   | Mod
   | {specLevel>0}? valMulOp
   ;

additiveExpression:
	multiplicativeExpression
	| additiveExpression Plus multiplicativeExpression
	| additiveExpression Minus multiplicativeExpression;

shiftExpression:
	additiveExpression
	| shiftExpression RightShift additiveExpression
	| shiftExpression LeftShift additiveExpression;


relationalExpression:
	shiftExpression
    | relationalExpression relationalOp shiftExpression
	;

relationalOp:
    (Less | Greater | LessEqual | GreaterEqual)
    | {specLevel>0}? valInOp
    ;

equalityExpression:
	relationalExpression
	| equalityExpression Equal relationalExpression
	| equalityExpression NotEqual relationalExpression;

andExpression:
    equalityExpression
    | andExpression And equalityExpression;

exclusiveOrExpression:
    andExpression
    | exclusiveOrExpression Caret andExpression;

inclusiveOrExpression:
	exclusiveOrExpression
	| inclusiveOrExpression Or exclusiveOrExpression;

logicalAndExpression:
	inclusiveOrExpression
	| logicalAndExpression logicalAndOp inclusiveOrExpression;

logicalAndOp
    : AndAnd
    | {specLevel>0}? valAndOp
    ;

logicalOrExpression:
	logicalAndExpression
	| logicalOrExpression OrOr logicalAndExpression
	| logicalOrExpression valAndOp logicalAndExpression;


implicationExpression
    :   logicalOrExpression implicationOp implicationExpression
    |   logicalOrExpression
    ;

implicationOp
    :   {specLevel>0}? valImpOp
    ;

conditionalExpression:
	implicationExpression
	| implicationExpression Question expression Colon assignmentExpression;

assignmentExpression:
	valEmbedWith? conditionalExpression valEmbedThen?
	| valEmbedWith? logicalOrExpression assignmentOperator initializerClause valEmbedThen?
	| throwExpression
    ;

assignmentOperator:
	Assign
	| StarAssign
	| DivAssign
	| ModAssign
	| PlusAssign
	| MinusAssign
	| RightShiftAssign
	| LeftShiftAssign
	| AndAssign
	| XorAssign
	| OrAssign;

expression:
    assignmentExpression
    | expression Comma assignmentExpression;

constantExpression: conditionalExpression;
/*Statements*/

statement:
	labeledStatement
	| attributeSpecifierSeq? expressionStatement
	| attributeSpecifierSeq? compoundStatement
	| attributeSpecifierSeq? selectionStatement
	| attributeSpecifierSeq? iterationStatement
	| attributeSpecifierSeq? jumpStatement
	| attributeSpecifierSeq? tryBlock
	| declarationStatement
    | {specLevel>0}? valEmbedStatementBlock
    | {specLevel>0}? valStatement
	;

labeledStatement:
	attributeSpecifierSeq? cppIdentifier Colon statement
	| attributeSpecifierSeq? Case constantExpression  Colon statement
	| attributeSpecifierSeq? Default Colon statement;


expressionStatement: expression? Semi;

compoundStatement: LeftBrace statementSeq? RightBrace;

statementSeq: statement+;

selectionStatement:
	If LeftParen condition RightParen statement Else statement
	| If LeftParen condition RightParen statement
	| Switch LeftParen condition RightParen statement;

condition:
	expression
	| attributeSpecifierSeq? declSpecifierSeq declarator Assign initializerClause
	| attributeSpecifierSeq? declSpecifierSeq declarator bracedInitList;

iterationStatement:
	valEmbedContract? While LeftParen condition RightParen valEmbedContract? statement
	| Do statement While LeftParen expression RightParen Semi
	| valEmbedContract? For LeftParen forInitStatement condition? Semi expression? RightParen valEmbedContract? statement
	| valEmbedContract? For LeftParen forRangeDeclaration Colon forRangeInitializer RightParen valEmbedContract? statement;

forInitStatement: expressionStatement | simpleDeclaration;

forRangeDeclaration:
	attributeSpecifierSeq? declSpecifierSeq declarator;

forRangeInitializer: expression | bracedInitList;

jumpStatement:
	Break Semi
    | Continue Semi
    | Return expression Semi
    | Return bracedInitList Semi
    | Return Semi
    | Goto cppIdentifier Semi;


declarationStatement: blockDeclaration;
/*Declarations*/

declarationseq: declaration+;

declaration:
	blockDeclaration
	| functionDefinition
	| templateDeclaration
	| explicitInstantiation
	| explicitSpecialization
	| linkageSpecification
	| namespaceDefinition
	| emptyDeclaration
	| attributeDeclaration;

blockDeclaration:
	simpleDeclaration
	| asmDefinition
	| namespaceAliasDefinition
	| usingDeclaration
	| usingDirective
	| staticAssertDeclaration
	| aliasDeclaration
	| opaqueEnumDeclaration;

aliasDeclaration:
	Using cppIdentifier attributeSpecifierSeq? Assign theTypeId Semi;

simpleDeclaration:
	valEmbedContract? declSpecifierSeq? initDeclaratorList? Semi
	| valEmbedContract? attributeSpecifierSeq declSpecifierSeq? initDeclaratorList Semi;

staticAssertDeclaration:
	Static_assert LeftParen constantExpression Comma StringLiteral RightParen Semi;

emptyDeclaration: Semi;

attributeDeclaration: attributeSpecifierSeq Semi;

declSpecifier:
	storageClassSpecifier
	| typeSpecifier
	| functionSpecifier
	| Friend
	| Typedef
	| Constexpr
    | valEmbedModifier // changed from valEmbedModifiers
	;

declSpecifierSeq: declSpecifier+ attributeSpecifierSeq?;

storageClassSpecifier:
	Register
	| Static
	| Thread_local
	| Extern
	| Mutable;

functionSpecifier: Inline | Virtual | Explicit;

typedefName: cppIdentifier;

typeSpecifier:
	trailingTypeSpecifier
	| classSpecifier
	| enumSpecifier;

trailingTypeSpecifier:
	simpleTypeSpecifier
	| elaboratedTypeSpecifier
	| typeNameSpecifier
	| cvQualifier;

typeSpecifierSeq: typeSpecifier+ attributeSpecifierSeq?;

trailingTypeSpecifierSeq:
	trailingTypeSpecifier+ attributeSpecifierSeq?;

simpleTypeSpecifier:
	nestedNameSpecifier? theTypeName
	| nestedNameSpecifier Template simpleTemplateId
	| Char
	| Char16
	| Char32
	| Wchar
	| Bool
	| Short
	| Int
	| Long
	| Signed
	| Unsigned
	| Float
	| Double
	| Void
	| Auto
	| decltypeSpecifier
    | {specLevel>0}? valType
    ;

theTypeName:
	className
	| enumName
	| typedefName
	| simpleTemplateId;

decltypeSpecifier:
	Decltype LeftParen expression RightParen
	| Decltype LeftParen Auto RightParen;

elaboratedTypeSpecifier:
    classKey attributeSpecifierSeq? nestedNameSpecifier? cppIdentifier
    | classKey simpleTemplateId
    | classKey nestedNameSpecifier Template? simpleTemplateId
	| Enum nestedNameSpecifier? cppIdentifier;

enumName: cppIdentifier;

enumSpecifier:
	enumHead LeftBrace enumeratorList Comma? RightBrace
	| enumHead LeftBrace RightBrace;

enumHead:
	enumkey attributeSpecifierSeq? nestedNameSpecifier? cppIdentifier enumbase?
	| enumkey attributeSpecifierSeq? enumbase?;

opaqueEnumDeclaration:
	enumkey attributeSpecifierSeq? cppIdentifier enumbase? Semi;

enumkey:
    Enum Class
    | Enum Struct
    | Enum ;

enumbase: Colon typeSpecifierSeq;

enumeratorList:
	enumeratorDefinition
	| enumeratorDefinition Comma enumeratorList;

enumeratorDefinition:
    enumerator Assign constantExpression
    | enumerator ;

enumerator: cppIdentifier;

namespaceName: originalNamespaceName | namespaceAlias;

originalNamespaceName: cppIdentifier;

namespaceDefinition:
	Inline? Namespace (cppIdentifier | originalNamespaceName)? LeftBrace namespaceBody = declarationseq
		? RightBrace;

namespaceAlias: cppIdentifier;

namespaceAliasDefinition:
	Namespace cppIdentifier Assign qualifiednamespacespecifier Semi;

qualifiednamespacespecifier: nestedNameSpecifier? namespaceName;

usingDeclaration:
	Using ((Typename_? nestedNameSpecifier) | Doublecolon) unqualifiedId Semi;

usingDirective:
	attributeSpecifierSeq? Using Namespace nestedNameSpecifier? namespaceName Semi;

asmDefinition: Asm LeftParen StringLiteral RightParen Semi;

linkageSpecification:
	Extern StringLiteral (
		LeftBrace declarationseq? RightBrace
		| declaration
	);

attributeSpecifierSeq: attributeSpecifier+;

attributeSpecifier:
	LeftBracket LeftBracket attributeList? RightBracket RightBracket
	| alignmentspecifier;

alignmentspecifier:
	Alignas LeftParen theTypeId Ellipsis? RightParen
	| Alignas LeftParen constantExpression Ellipsis? RightParen;

attributeList:
    attribute Ellipsis?
    | attribute Comma attributeList;

attribute:
    attributeNamespace Doublecolon cppIdentifier attributeArgumentClause?
    | cppIdentifier attributeArgumentClause?;

attributeNamespace: cppIdentifier;

attributeArgumentClause: LeftParen balancedTokenSeq? RightParen;

balancedTokenSeq: balancedtoken+;

balancedtoken:
	LeftParen balancedTokenSeq RightParen
	| LeftBracket balancedTokenSeq RightBracket
	| LeftBrace balancedTokenSeq RightBrace
	| ~(
		LeftParen
		| RightParen
		| LeftBrace
		| RightBrace
		| LeftBracket
		| RightBracket
	)+;
/*Declarators*/

initDeclaratorList:
    initDeclarator
    | initDeclarator Comma initDeclaratorList;

initDeclarator: declarator initializer?;

declarator:
	pointerDeclarator
	| noPointerDeclarator parametersAndQualifiers trailingReturnType;

pointerDeclarator:
    pointerOperator Const? noPointerDeclarator
    | pointerOperator Const? pointerDeclarator;

noPointerDeclarator:
	declaratorid attributeSpecifierSeq?
	| noPointerDeclarator parametersAndQualifiers
	| noPointerDeclarator LeftBracket constantExpression? RightBracket attributeSpecifierSeq?
	| LeftParen pointerDeclarator RightParen;

parametersAndQualifiers:
	LeftParen parameterDeclarationClause? RightParen cvqualifierseq? refqualifier?
		exceptionSpecification? attributeSpecifierSeq?;

trailingReturnType:
	Arrow trailingTypeSpecifierSeq abstractDeclarator?;

pointerOperator:
	(And | AndAnd) attributeSpecifierSeq?
	| nestedNameSpecifier? Star attributeSpecifierSeq? cvqualifierseq?;

cvqualifierseq: cvQualifier+;

cvQualifier: Const | Volatile;

refqualifier: And | AndAnd;

declaratorid: Ellipsis? idExpression;

theTypeId: typeSpecifierSeq abstractDeclarator?;

abstractDeclarator:
	pointerAbstractDeclarator
	| noPointerAbstractDeclarator? parametersAndQualifiers trailingReturnType
	| abstractPackDeclarator;

pointerAbstractDeclarator:
	noPointerAbstractDeclarator
	| pointerOperator+ noPointerAbstractDeclarator?;

noPointerAbstractDeclarator:
	noPointerAbstractDeclarator parametersAndQualifiers
	| noPointerAbstractDeclarator noPointerAbstractDeclarator LeftBracket constantExpression? RightBracket attributeSpecifierSeq?
	| parametersAndQualifiers
	| LeftBracket constantExpression? RightBracket attributeSpecifierSeq?
	| LeftParen pointerAbstractDeclarator RightParen;

abstractPackDeclarator:
	pointerOperator* noPointerAbstractPackDeclarator;

noPointerAbstractPackDeclarator:
	noPointerAbstractPackDeclarator parametersAndQualifiers
	| noPointerAbstractPackDeclarator LeftBracket constantExpression? RightBracket attributeSpecifierSeq?
	| Ellipsis;

parameterDeclarationClause:
	parameterDeclarationList (Comma? Ellipsis)?;

parameterDeclarationList:
	parameterDeclaration
	| parameterDeclaration Comma parameterDeclarationList;

parameterDeclaration:
	attributeSpecifierSeq? declSpecifierSeq (
		(declarator | abstractDeclarator?) (
			Assign initializerClause
		)?
	);

functionDefinition:
	valEmbedContract? attributeSpecifierSeq? declSpecifierSeq? declarator virtualSpecifierSeq? functionBody;

functionBody:
	constructorInitializer? compoundStatement
	| functionTryBlock
	| Assign (Default | Delete) Semi;

initializer:
	braceOrEqualInitializer
	| LeftParen expressionList RightParen;

braceOrEqualInitializer:
	Assign initializerClause
	| bracedInitList;

initializerClause: assignmentExpression | bracedInitList;

initializerList:
	initializerClause Ellipsis?
	| initializerClause Ellipsis? Comma initializerList;

bracedInitList:
    LeftBrace initializerList Comma? RightBrace
    | LeftBrace RightBrace;

/*Classes*/

className: cppIdentifier | simpleTemplateId;

classSpecifier:
	classHead LeftBrace memberSpecification? RightBrace;

classHead:
	classKey attributeSpecifierSeq? classHeadName classVirtSpecifier? baseClause?
	| classKey attributeSpecifierSeq? baseClause?
	| Union attributeSpecifierSeq? classHeadName classVirtSpecifier?
	| Union attributeSpecifierSeq? ;

classHeadName: nestedNameSpecifier? className;

classVirtSpecifier: Final;

classKey: Class | Struct;

memberSpecification:
	memberdeclaration
    | accessSpecifier
	| memberdeclaration memberSpecification
	| accessSpecifier Colon memberSpecification;

memberdeclaration:
	attributeSpecifierSeq? declSpecifierSeq? memberDeclaratorList? Semi
	| functionDefinition
	| usingDeclaration
	| staticAssertDeclaration
	| templateDeclaration
	| aliasDeclaration
	| emptyDeclaration;

memberDeclaratorList:
	memberDeclarator
	| memberDeclarator Comma memberDeclaratorList;

memberDeclarator:
	declarator (
		virtualSpecifierSeq? pureSpecifier?
		| braceOrEqualInitializer?
	)
	| cppIdentifier? attributeSpecifierSeq? Colon constantExpression;

virtualSpecifierSeq: virtualSpecifier+;

virtualSpecifier: Override | Final;
/*
 purespecifier: Assign '0'//Conflicts with the lexer ;
 */

pureSpecifier:
	Assign val = OctalLiteral {if($val.text.compareTo("0")!=0) throw new InputMismatchException(this);};
/*Derived classes*/

baseClause: Colon baseSpecifierList;

baseSpecifierList:
	baseSpecifier Ellipsis?
	| baseSpecifier Ellipsis? Comma baseSpecifierList;

baseSpecifier:
	attributeSpecifierSeq? accessSpecifier Virtual? baseTypeSpecifier
	| attributeSpecifierSeq? Virtual accessSpecifier? baseTypeSpecifier
	| attributeSpecifierSeq? accessSpecifier Virtual? baseTypeSpecifier;

classOrDeclType:
	nestedNameSpecifier? className
	| decltypeSpecifier;

baseTypeSpecifier: classOrDeclType;

accessSpecifier: Private | Protected | Public;
/*Special member functions*/

conversionFunctionId: Operator conversionTypeId;

conversionTypeId: typeSpecifierSeq conversionDeclarator?;

conversionDeclarator: pointerOperator conversionDeclarator?;

constructorInitializer: Colon memInitializerList;

memInitializerList:
	memInitializer Ellipsis?
	| memInitializer Ellipsis? Comma memInitializerList;

memInitializer:
	meminitializerid LeftParen expressionList? RightParen
	| meminitializerid bracedInitList;

meminitializerid: classOrDeclType | cppIdentifier;
/*Overloading*/

operatorFunctionId: Operator theOperator;

literalOperatorId:
	Operator StringLiteral cppIdentifier
	| Operator UserDefinedStringLiteral;

/*Templates*/

templateDeclaration:
	Template Less templateparameterList Greater declaration;

templateparameterList:
	templateParameter
	| templateParameter Comma templateparameterList;

templateParameter: typeParameter | parameterDeclaration;

typeParameter:
	Class Ellipsis? cppIdentifier?
	| Template Less templateparameterList Greater Class Ellipsis? cppIdentifier?
	| Typename_ Ellipsis? cppIdentifier?
	| Class cppIdentifier? Assign theTypeId
	| Template Less templateparameterList Greater cppIdentifier? Assign theTypeId
	| Typename_ cppIdentifier? Assign theTypeId;

simpleTemplateId:
	templateName Less templateArgumentList? Greater;

templateId:
	simpleTemplateId
	| operatorFunctionId Less templateArgumentList? Greater
	| literalOperatorId Less templateArgumentList? Greater;

templateName: cppIdentifier;

templateArgumentList:
	templateArgument Ellipsis?
	| templateArgument Ellipsis? Comma templateArgumentList;

templateArgument: theTypeId | constantExpression | idExpression;

typeNameSpecifier:
	Typename_ nestedNameSpecifier cppIdentifier
	| Typename_ nestedNameSpecifier Template? simpleTemplateId;

explicitInstantiation: Extern? Template declaration;

explicitSpecialization: Template Less Greater declaration;
/*Exception handling*/

tryBlock: Try compoundStatement handlerSeq;

functionTryBlock:
	Try constructorInitializer? compoundStatement handlerSeq;

handlerSeq: handler+;

handler:
	Catch LeftParen exceptionDeclaration RightParen compoundStatement;

exceptionDeclaration:
	attributeSpecifierSeq? typeSpecifierSeq declarator
	| attributeSpecifierSeq? typeSpecifierSeq abstractDeclarator
	| attributeSpecifierSeq? typeSpecifierSeq
	| Ellipsis;

throwExpression: Throw assignmentExpression?;

exceptionSpecification:
	dynamicExceptionSpecification
	| noeExceptSpecification;

dynamicExceptionSpecification:
	Throw LeftParen typeIdList? RightParen;

typeIdList:
    theTypeId Ellipsis?
    | theTypeId Ellipsis? Comma typeIdList;

noeExceptSpecification:
	Noexcept LeftParen constantExpression RightParen
	| Noexcept;
/*Preprocessing directives*/

/*Lexer*/

theOperator:
	New
	| New LeftBracket RightBracket
	| Delete
	| Delete LeftBracket RightBracket
	| Plus
	| Minus
	| Star
	| Div
	| Mod
	| Caret
	| And
	| Or
	| Tilde
	| Not
	| Assign
	| Less
	| GreaterEqual
	| PlusAssign
	| MinusAssign
	| StarAssign
	| Assign
	| ModAssign
	| XorAssign
	| AndAssign
	| OrAssign
	| LeftShift
	| RightShift
	| RightShiftAssign
	| LeftShiftAssign
	| Equal
	| NotEqual
	| LessEqual
	| GreaterEqual
	| AndAnd
	| OrOr
	| PlusPlus
	| MinusMinus
	| Comma
	| ArrowStar
	| Arrow
	| LeftParen RightParen
	| LeftBracket RightBracket;
	
cppIdentifier
    :   Identifier
    |   valIdentifier
    ;
