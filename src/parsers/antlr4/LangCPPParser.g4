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

// Grammar for C++ 14
parser grammar LangCPPParser;

// Root
translationUnit: declarationseq? EOF;

// Identifiers
clangppIdentifier:
    Identifier
    | valIdentifier;

// Expressions
primaryExpression:
    valExpr
	| literal // EW: Changed to match only one literal to prevent parsing errors
	| This
	| LeftParen expression RightParen
	| idExpression
	| lambdaExpression;

annotatedPrimaryExpression: valEmbedWith? primaryExpression valEmbedThen?;

idExpression: unqualifiedId | qualifiedId;

unqualifiedId:
	templateId
	| clangppIdentifier
	| operatorFunctionId
	| conversionFunctionId
	| literalOperatorId
	| Tilde (className | decltypeSpecifier);

qualifiedId: nestedNameSpecifier Template? unqualifiedId;

nestedNameSpecifier:
	theTypeName Doublecolon
	| namespaceName Doublecolon
	| decltypeSpecifier Doublecolon
	| Doublecolon
	| nestedNameSpecifier clangppIdentifier Doublecolon
	| nestedNameSpecifier Template? simpleTemplateId Doublecolon;

lambdaExpression:
	valEmbedContract? lambdaIntroducer lambdaDeclarator? compoundStatement;

lambdaIntroducer: LeftBracket lambdaCapture? RightBracket;

lambdaCapture:
	captureList
	| captureDefault (Comma captureList)?;

captureDefault: And | Assign;

captureList: capture (Comma capture)* Ellipsis?;

capture: simpleCapture | initcapture;

simpleCapture: And? clangppIdentifier | This;

initcapture: And? clangppIdentifier initializer;

lambdaDeclarator:
	LeftParen parameterDeclarationClause? RightParen Mutable? exceptionSpecification?
		attributeSpecifierSeq? trailingReturnType?;

postfixExpression:
	annotatedPrimaryExpression
	| postfixExpression LeftBracket expression RightBracket
	| postfixExpression LeftBracket bracedInitList RightBracket
	| postfixExpression LeftParen expressionList? RightParen valEmbedGiven? valEmbedYields?
	| postfixExpression Dot Template? idExpression
	| postfixExpression Dot pseudoDestructorName
	| postfixExpression Arrow Template? idExpression
	| postfixExpression Arrow pseudoDestructorName
	| postfixExpression PlusPlus
	| postfixExpression MinusMinus
	| postfixExpression specPostfix
	| simpleTypeSpecifier LeftParen expressionList? RightParen valEmbedGiven? valEmbedYields?
	| simpleTypeSpecifier bracedInitList
	| typeNameSpecifier LeftParen expressionList? RightParen valEmbedGiven? valEmbedYields?
	| typeNameSpecifier bracedInitList
	| (
		Dynamic_cast
		| Static_cast
		| Reinterpret_cast
		| Const_cast
	) Less theTypeId Greater LeftParen expression RightParen
	| typeIdOfTheTypeId LeftParen (expression | theTypeId) RightParen;

specPostfix: {specLevel>0}? valPostfix;

/*
 add a middle layer to eliminate duplicated function declarations
 */

typeIdOfTheTypeId: Typeid_;

expressionList: initializerList;

pseudoDestructorName:
	nestedNameSpecifier? (theTypeName Doublecolon)? Tilde theTypeName
	| nestedNameSpecifier Template simpleTemplateId Doublecolon Tilde theTypeName
	| Tilde decltypeSpecifier;

unaryExpression:
	postfixExpression
	| PlusPlus unaryExpression
	| MinusMinus unaryExpression
	| unaryOperator unaryExpression
	| Sizeof unaryExpression
	| Sizeof (
		LeftParen theTypeId RightParen
		| Ellipsis LeftParen clangppIdentifier RightParen
	)
	| Alignof LeftParen theTypeId RightParen
	| noExceptExpression
	| newExpression
	| deleteExpression
	| specPrefix unaryExpression;

specPrefix: {specLevel>0}? valPrefix;

unaryOperator: And | Star | Plus | Minus | Tilde | Not | NotWord;

newExpression:
	Doublecolon? New newPlacement? newTypePtr newInitializer? valEmbedGiven? valEmbedYields?;

newPlacement: LeftParen expressionList RightParen;

newTypePtr:
    newTypeId
	| LeftParen theTypeId RightParen;

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

pointerMemberExpression:
    prependExpression
	| pointerMemberExpression DotStar prependExpression
	| pointerMemberExpression ArrowStar prependExpression;

prependExpression:
		castExpression
    | castExpression prependOp prependExpression;

prependOp: {specLevel>0}? valPrependOp;

multiplicativeExpression:
    pointerMemberExpression
	| multiplicativeExpression multiplicativeOp pointerMemberExpression;

multiplicativeOp:
    Star
    | Div
    | Mod
    | {specLevel>0}? valMulOp;

additiveExpression:
    multiplicativeExpression
	| additiveExpression Plus multiplicativeExpression
	| additiveExpression Minus multiplicativeExpression;

shiftExpression:
	additiveExpression
	| shiftExpression Less Less additiveExpression
	| shiftExpression Greater Greater additiveExpression;

relationalExpression:
    shiftExpression
    | relationalExpression relationalOp shiftExpression;

relationalOp:
    (Less | Greater | LessEqual | GreaterEqual)
    |   {specLevel>0}? valInOp;

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

logicalAndOp:
    AndAnd
    | {specLevel>0}? valAndOp;

logicalOrExpression:
	logicalAndExpression
	| logicalOrExpression OrOr logicalAndExpression;

implicationExpression:
		logicalOrExpression
    | logicalOrExpression implicationOp implicationExpression;

implicationOp: {specLevel>0}? valImpOp;

conditionalExpression:
	implicationExpression
	| implicationExpression Question expression Colon assignmentExpression;

assignmentExpression:
	valEmbedWith? conditionalExpression valEmbedThen?
	| valEmbedWith? logicalOrExpression assignmentOperator initializerClause valEmbedThen?
	| throwExpression;

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

// Statements
statement:
  attributeSpecifierSeq? statementTwo
  | blockDeclaration
	| labeledStatement;

statementTwo:
    expressionStatement
    | compoundStatement
    | selectionStatement
    | iterationStatement
    | jumpStatement
    | tryBlock
    | valEmbedStatementBlock
    | {specLevel>0}? valStatement;

labeledStatement:
	attributeSpecifierSeq? (
		clangppIdentifier
		| Case constantExpression
		| Default
	) Colon statement;

expressionStatement: expression? Semi;

compoundStatement: LeftBrace statementSeq? RightBrace;

statementSeq: statement+;

selectionStatement:
	ifStatement
	| switchStatement;

ifStatement:
	If LeftParen condition RightParen statement Else statement
	| If LeftParen condition RightParen statement;

switchStatement: Switch LeftParen condition RightParen statement;

condition:
	expression
	| attributeSpecifierSeq? declSpecifierSeq declarator (
		Assign initializerClause
		| bracedInitList
	);

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
	| Goto clangppIdentifier Semi;

// Declarations
declarationseq:
	declaration Semi*
	| declarationseq declaration Semi*;

declaration:
	functionDefinition
	| blockDeclaration
	| templateDeclaration
	| explicitInstantiation
	| explicitSpecialization
	| linkageSpecification
	| namespaceDefinition
	| emptyDeclaration
	| attributeDeclaration
	| valEmbedGlobalDeclarationBlock;

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
	Using clangppIdentifier attributeSpecifierSeq? Assign theTypeId Semi;

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
	| valEmbedModifier;

declSpecifierSeq: declSpecifier+? attributeSpecifierSeq?;

storageClassSpecifier:
	Register
	| Static
	| Thread_local
	| Extern
	| Mutable;

functionSpecifier: Inline | Virtual | Explicit;

typedefName: clangppIdentifier;

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

simpleTypeLengthModifier:
	Short
	| Long;

simpleTypeSignednessModifier:
	Unsigned
	| Signed;

simpleTypeSpecifier:
	nestedNameSpecifier? theTypeName
	| nestedNameSpecifier Template simpleTemplateId
	| simpleTypeSignednessModifier
	| simpleTypeSignednessModifier? simpleTypeLengthModifier+
	| simpleTypeSignednessModifier? Char
	| simpleTypeSignednessModifier? Char16
	| simpleTypeSignednessModifier? Char32
	| simpleTypeSignednessModifier? Wchar
	| Bool
	| simpleTypeSignednessModifier? simpleTypeLengthModifier* Int
	| Float
	| simpleTypeLengthModifier? Double
	| Void
	| Auto
	| {specLevel>0}? valType
	| decltypeSpecifier;

theTypeName:
	simpleTemplateId
	| className
	| enumName
	| typedefName;

decltypeSpecifier:
	Decltype LeftParen (expression | Auto) RightParen;

elaboratedTypeSpecifier:
	classKey (
		attributeSpecifierSeq? nestedNameSpecifier? clangppIdentifier
		| simpleTemplateId
		| nestedNameSpecifier Template? simpleTemplateId
	)
	| Enum nestedNameSpecifier? clangppIdentifier;

enumName: clangppIdentifier;

enumSpecifier:
	enumHead LeftBrace (enumeratorList Comma?)? RightBrace;

enumHead:
	enumkey attributeSpecifierSeq? (
		nestedNameSpecifier? clangppIdentifier
	)? enumbase?;

opaqueEnumDeclaration:
	enumkey attributeSpecifierSeq? clangppIdentifier enumbase? Semi;

enumkey: Enum (Class | Struct)?;

enumbase: Colon typeSpecifierSeq;

enumeratorList:
	enumeratorDefinition (Comma enumeratorDefinition)*;

enumeratorDefinition: enumerator (Assign constantExpression)?;

enumerator: clangppIdentifier;

namespaceName: originalNamespaceName | namespaceAlias;

originalNamespaceName: clangppIdentifier;

namespaceDefinition:
	Inline? Namespace clangppIdentifier? LeftBrace declarationseq? RightBrace;

namespaceAlias: clangppIdentifier;

namespaceAliasDefinition:
	Namespace clangppIdentifier Assign qualifiednamespacespecifier Semi;

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
	Alignas LeftParen (theTypeId | constantExpression) Ellipsis? RightParen;

attributeList: attribute (Comma attribute)* Ellipsis?;

attribute: (attributeNamespace Doublecolon)? clangppIdentifier attributeArgumentClause?;

attributeNamespace: clangppIdentifier;

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

// Declarators
initDeclaratorList:
    initDeclarator
    | initDeclaratorList Comma initDeclarator;

initDeclarator: declarator initializer?;

declarator:
	pointerDeclarator
	| noPointerDeclarator parametersAndQualifiers trailingReturnType;

pointerDeclarator: pointerDeclaratorPrefix* noPointerDeclarator;

pointerDeclaratorPrefix: pointerOperatorWithDoubleStar Const?;

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
	And attributeSpecifierSeq?
	| AndAnd attributeSpecifierSeq?
	| nestedNameSpecifier? Star attributeSpecifierSeq? cvqualifierseq?;

// ** is tokenized separately as separating conjunction,
// so add special case where ** can be used, which is when
// multiple pointerOperators are allowed to be repeated after each other
pointerOperatorWithDoubleStar:
    pointerOperator
    | nestedNameSpecifier? SEP_CONJ attributeSpecifierSeq? cvqualifierseq?;

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
	| pointerOperatorWithDoubleStar+ noPointerAbstractDeclarator?;

noPointerAbstractDeclarator:
	noPointerAbstractDeclarator (
		parametersAndQualifiers
		| noPointerAbstractDeclarator LeftBracket constantExpression? RightBracket
			attributeSpecifierSeq?
	)
	| parametersAndQualifiers
	| LeftBracket constantExpression? RightBracket attributeSpecifierSeq?
	| LeftParen pointerAbstractDeclarator RightParen;

abstractPackDeclarator:
	pointerOperatorWithDoubleStar* noPointerAbstractPackDeclarator;

noPointerAbstractPackDeclarator:
	noPointerAbstractPackDeclarator (
		parametersAndQualifiers
		| LeftBracket constantExpression? RightBracket attributeSpecifierSeq?
	)
	| Ellipsis;

parameterDeclarationClause:
	parameterDeclarationList parameterDeclarationVarargs?;

parameterDeclarationVarargs: Comma? Ellipsis;

parameterDeclarationList:
	parameterDeclaration (Comma parameterDeclaration)*;

parameterDeclaration:
  declSpecifierSeq declarator
	| attributeSpecifierSeq? declSpecifierSeq (
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

// EW: Had to flip the two options to not get parsing errors
initializerClause: bracedInitList | assignmentExpression;

initializerList:
	initializerClause Ellipsis?
	| initializerList Comma initializerClause Ellipsis?;

bracedInitList:
    LeftBrace RightBrace
    | LeftBrace initializerList RightBrace
    | LeftBrace initializerList Comma RightBrace;

// Classes
className: clangppIdentifier | simpleTemplateId;

classSpecifier:
	classHead LeftBrace memberSpecification? RightBrace;

classHead:
	classKey attributeSpecifierSeq? (
		classHeadName classVirtSpecifier?
	)? baseClause?
	| Union attributeSpecifierSeq? (
		classHeadName classVirtSpecifier?
	)?;

classHeadName: nestedNameSpecifier? className;

classVirtSpecifier: Final;

classKey: Class | Struct;

memberSpecification:
	(memberdeclaration | accessSpecifier Colon)+;

memberdeclaration:
	attributeSpecifierSeq? declSpecifierSeq? memberDeclaratorList? Semi
	| functionDefinition
	| usingDeclaration
	| staticAssertDeclaration
	| templateDeclaration
	| aliasDeclaration
	| emptyDeclaration;

memberDeclaratorList:
	memberDeclarator (Comma memberDeclarator)*;

memberDeclarator:
	declarator (
		virtualSpecifierSeq? pureSpecifier?
		| braceOrEqualInitializer?
	)
	| clangppIdentifier? attributeSpecifierSeq? Colon constantExpression;

virtualSpecifierSeq: virtualSpecifier+;

virtualSpecifier: Override | Final;
/*
 purespecifier: Assign '0'//Conflicts with the lexer ;
 */

pureSpecifier:
	Assign val = OctalLiteral {if($val.text.compareTo("0")!=0) throw new InputMismatchException(this);
		};

//Derived classes
baseClause: Colon baseSpecifierList;

baseSpecifierList:
	baseSpecifier Ellipsis? (Comma baseSpecifier Ellipsis?)*;

baseSpecifier:
	attributeSpecifierSeq? (
		baseTypeSpecifier
		| Virtual accessSpecifier? baseTypeSpecifier
		| accessSpecifier Virtual? baseTypeSpecifier
	);

classOrDeclType:
	nestedNameSpecifier? className
	| decltypeSpecifier;

baseTypeSpecifier: classOrDeclType;

accessSpecifier: Private | Protected | Public;

// Special member functions
conversionFunctionId: Operator conversionTypeId;

conversionTypeId: typeSpecifierSeq conversionDeclarator?;

conversionDeclarator: pointerOperator conversionDeclarator?;

constructorInitializer: Colon memInitializerList;

memInitializerList:
	memInitializer Ellipsis? (Comma memInitializer Ellipsis?)*;

memInitializer:
	meminitializerid (
		LeftParen expressionList? RightParen
		| bracedInitList
	);

meminitializerid: classOrDeclType | clangppIdentifier;

// Overloading
operatorFunctionId: Operator theOperator;

literalOperatorId:
	Operator (
		StringLiteral clangppIdentifier
		| UserDefinedStringLiteral
	);

// Templates
templateDeclaration:
	Template Less templateparameterList Greater declaration;

templateparameterList:
	templateParameter (Comma templateParameter)*;

templateParameter: typeParameter | parameterDeclaration;

typeParameter:
	(
		(Template Less templateparameterList Greater)? Class
		| Typename_
	) ((Ellipsis? clangppIdentifier?) | (clangppIdentifier? Assign theTypeId));

simpleTemplateId:
	templateName Less templateArgument Greater
	| templateName Less templateArgumentList? Greater;

templateId:
	simpleTemplateId
	| (operatorFunctionId | literalOperatorId) Less templateArgumentList? Greater;

templateName: clangppIdentifier;

templateArgumentList:
	templateArgument Ellipsis? Comma templateArgumentList
	| templateArgument Ellipsis?;

templateArgument: theTypeId | constantExpression | idExpression;

typeNameSpecifier:
	Typename_ nestedNameSpecifier (
		clangppIdentifier
		| Template? simpleTemplateId
	);

explicitInstantiation: Extern? Template declaration;

explicitSpecialization: Template Less Greater declaration;

// Exception handling
tryBlock: Try compoundStatement handlerSeq;

functionTryBlock:
	Try constructorInitializer? compoundStatement handlerSeq;

handlerSeq: handler+;

handler:
	Catch LeftParen exceptionDeclaration RightParen compoundStatement;

exceptionDeclaration:
	attributeSpecifierSeq? typeSpecifierSeq (
		declarator
		| abstractDeclarator
	)?
	| Ellipsis;

throwExpression: Throw assignmentExpression?;

exceptionSpecification:
	dynamicExceptionSpecification
	| noeExceptSpecification;

dynamicExceptionSpecification:
	Throw LeftParen typeIdList? RightParen;

typeIdList: theTypeId Ellipsis? (Comma theTypeId Ellipsis?)*;

noeExceptSpecification:
	Noexcept LeftParen constantExpression RightParen
	| Noexcept;

// Preprocessing directives

// Lexer
theOperator:
	New (LeftBracket RightBracket)?
	| Delete (LeftBracket RightBracket)?
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
	| NotWord
	| Assign
	| Greater
	| Less
	| GreaterEqual
	| PlusAssign
	| MinusAssign
	| StarAssign
	| ModAssign
	| XorAssign
	| AndAssign
	| OrAssign
	| Less Less
	| Greater Greater
	| RightShiftAssign
	| LeftShiftAssign
	| Equal
	| NotEqual
	| LessEqual
	| AndAnd
	| OrOr
	| PlusPlus
	| MinusMinus
	| Comma
	| ArrowStar
	| Arrow
	| LeftParen RightParen
	| LeftBracket RightBracket;

literal:
	IntegerLiteral
	| CharacterLiteral
	| FloatingLiteral
	| StringLiteral
	| BooleanLiteral
	| PointerLiteral
	| UserDefinedLiteral;

