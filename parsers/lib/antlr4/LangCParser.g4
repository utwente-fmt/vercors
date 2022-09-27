/*
 [The "BSD licence"]
 Copyright (c) 2013 Sam Harwell
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

/** C 2011 grammar built from the C11 Spec */
parser grammar C;
import LangOMPParser, LangGPGPUParser;

primaryExpression
    :   valExpr
    |   clangIdentifier
    |   Constant
    |   StringLiteral+
    |   '(' expression ')'
    |   genericSelection
    |   '__extension__'? '(' compoundStatement ')' // Blocks (GCC extension)
    |   '__builtin_va_arg' '(' unaryExpression ',' typeName ')'
    |   '__builtin_offsetof' '(' typeName ',' unaryExpression ')'
    |   'NULL'
    ;

annotatedPrimaryExpression
    : valEmbedWith? primaryExpression valEmbedThen?
    ;

genericSelection
    :   '_Generic' '(' assignmentExpression ',' genericAssocList ')'
    ;

genericAssocList
    :   genericAssociation
    |   genericAssocList ',' genericAssociation
    ;

genericAssociation
    :   typeName ':' assignmentExpression
    |   'default' ':' assignmentExpression
    ;

postfixExpression
    :   annotatedPrimaryExpression
    |   postfixExpression '[' expression ']'
    |   postfixExpression '(' argumentExpressionList? ')' valEmbedGiven? valEmbedYields?
    |   postfixExpression '.' clangIdentifier
    |   postfixExpression '->' clangIdentifier
    |   postfixExpression '++'
    |   postfixExpression '--'
    |   postfixExpression specPostfix
    |   '(' typeName ')' '{' initializerList '}'
    |   '(' typeName ')' '{' initializerList ',' '}'
    |   '__extension__' '(' typeName ')' '{' initializerList '}'
    |   '__extension__' '(' typeName ')' '{' initializerList ',' '}'
    |   gpgpuCudaKernelInvocation
    ;

specPostfix
    :   {specLevel>0}? valPostfix
    ;

argumentExpressionList
    :   assignmentExpression
    |   argumentExpressionList ',' assignmentExpression
    ;

unaryExpression
    :   postfixExpression
    |   '++' unaryExpression
    |   '--' unaryExpression
    |   unaryOperator castExpression
    |   'sizeof' unaryExpression
    |   'sizeof' '(' typeName ')'
    |   '_Alignof' '(' typeName ')'
    |   '&&'  clangIdentifier // GCC extension address of label
    ;

unaryOperator
    :   ('&' | '*' | '+' | '-' | '~' | '!')
    ;

castExpression
    :   unaryExpression
    |   '(' typeName ')' castExpression
    |   '__extension__' '(' typeName ')' castExpression
    ;

prependExpression
    :   castExpression prependOp prependExpression
    |   castExpression
    ;

prependOp
    :   {specLevel>0}? valPrependOp
    ;

multiplicativeExpression
    :   prependExpression
    |   multiplicativeExpression multiplicativeOp prependExpression
    ;

multiplicativeOp
    : '*'
    | '/'
    | '%'
    | {specLevel>0}? valMulOp
    ;

additiveExpression
    :   multiplicativeExpression
    |   additiveExpression '+' multiplicativeExpression
    |   additiveExpression '-' multiplicativeExpression
    ;

shiftExpression
    :   additiveExpression
    |   shiftExpression '<<' additiveExpression
    |   shiftExpression '>>' additiveExpression
    ;

relationalExpression
    :   shiftExpression
    |   relationalExpression relationalOp shiftExpression
    ;

relationalOp
    :   ('<'|'>'|'<='|'>=')
    |   {specLevel>0}? valInOp
    ;

equalityExpression
    :   relationalExpression
    |   equalityExpression '==' relationalExpression
    |   equalityExpression '!=' relationalExpression
    ;

andExpression
    :   equalityExpression
    |   andExpression '&' equalityExpression
    ;

exclusiveOrExpression
    :   andExpression
    |   exclusiveOrExpression '^' andExpression
    ;

inclusiveOrExpression
    :   exclusiveOrExpression
    |   inclusiveOrExpression '|' exclusiveOrExpression
    ;

logicalAndExpression
    :   inclusiveOrExpression
    |   logicalAndExpression logicalAndOp inclusiveOrExpression
    ;

logicalAndOp
    : '&&'
    | {specLevel>0}? valAndOp
    ;

logicalOrExpression
    :   logicalAndExpression
    |   logicalOrExpression '||' logicalAndExpression
    ;

implicationExpression
    :   logicalOrExpression implicationOp implicationExpression
    |   logicalOrExpression
    ;

implicationOp
    :   {specLevel>0}? valImpOp
    ;

conditionalExpression
    :   implicationExpression
    |   implicationExpression '?' expression ':' conditionalExpression
    ;

assignmentExpression
    :   valEmbedWith? conditionalExpression valEmbedThen?
    |   valEmbedWith? unaryExpression assignmentOperator assignmentExpression valEmbedThen?
    ;

assignmentOperator
    :   ('=' | '*=' | '/=' | '%=' | '+=' | '-=' | '<<=' | '>>=' | '&=' | '^=' | '|=')
    ;

expression
    :   assignmentExpression
    |   expression ',' assignmentExpression
    ;

constantExpression
    :   conditionalExpression
    ;

declaration
    :   valEmbedContract? declarationSpecifiers initDeclaratorList? ';'
    |   staticAssertDeclaration
    ;

declarationSpecifiers
    // Non-greedy, because otherwise the name of a variable would be parsed as the typedefName of the type
    :   declarationSpecifier+?
    ;

declarationSpecifiers2
    :   declarationSpecifier+?
    ;

declarationSpecifier
    :   storageClassSpecifier
    |   typeSpecifier
    |   typeQualifier
    |   functionSpecifier
    |   alignmentSpecifier
    |   gpgpuKernelSpecifier
    |   valEmbedModifier
    ;

initDeclaratorList
    :   initDeclarator
    |   initDeclaratorList ',' initDeclarator
    ;

initDeclarator
    :   declarator
    |   declarator '=' initializer
    ;

storageClassSpecifier
    :   'typedef'
    |   'extern'
    |   'static'
    |   '_Thread_local'
    |   'auto'
    |   'register'
    | gpgpuLocalMemory
    | gpgpuGlobalMemory
    ;

typeSpecifier
    :   ('void'
    |   'char'
    |   'short'
    |   'int'
    |   'long'
    |   'float'
    |   'double'
    |   'signed'
    |   'unsigned'
    |   '_Bool'
    |   '_Complex'
    |   '__m128'
    |   '__m128d'
    |   '__m128i')
    |   '__extension__' '(' ('__m128' | '__m128d' | '__m128i') ')'
    |   {specLevel>0}? valType
    |   atomicTypeSpecifier
    |   structOrUnionSpecifier
    |   enumSpecifier
    |   typedefName
    |   '__typeof__' '(' constantExpression ')' // GCC extension
    ;

structOrUnionSpecifier
    :   structOrUnion  clangIdentifier? '{' structDeclarationList '}'
    |   structOrUnion  clangIdentifier
    ;

structOrUnion
    :   'struct'
    |   'union'
    ;

structDeclarationList
    :   structDeclaration
    |   structDeclarationList structDeclaration
    ;

structDeclaration
    :   specifierQualifierList structDeclaratorList? ';'
    |   staticAssertDeclaration
    ;

specifierQualifierList
    :   typeSpecifier specifierQualifierList?
    |   typeQualifier specifierQualifierList?
    ;

structDeclaratorList
    :   structDeclarator
    |   structDeclaratorList ',' structDeclarator
    ;

structDeclarator
    :   declarator
    |   declarator? ':' constantExpression
    ;

enumSpecifier
    :   'enum' clangIdentifier? '{' enumeratorList '}'
    |   'enum' clangIdentifier? '{' enumeratorList ',' '}'
    |   'enum' clangIdentifier
    ;

enumeratorList
    :   enumerator
    |   enumeratorList ',' enumerator
    ;

enumerator
    :   enumerationConstant
    |   enumerationConstant '=' constantExpression
    ;

enumerationConstant
    :   clangIdentifier
    ;

atomicTypeSpecifier
    :   '_Atomic' '(' typeName ')'
    ;

typeQualifier
    :   'const'
    |   'restrict'
    |   'volatile'
    |   '_Atomic'
    ;

functionSpecifier
    :   ('inline'
    |   '_Noreturn'
    |   '__inline__' // GCC extension
    |   '__stdcall')
    |   gccAttributeSpecifier
    |   '__declspec' '(' clangIdentifier ')'
    ;

alignmentSpecifier
    :   '_Alignas' '(' typeName ')'
    |   '_Alignas' '(' constantExpression ')'
    ;

declarator
    :   pointer? directDeclarator gccDeclaratorExtension*
    ;

directDeclarator
    :   clangIdentifier
    // |   '(' declarator ')'
    // Unsupported: this mixes with function calls really poorly: e.g. int(x); is a valid declaration of x as type int.
    |   directDeclarator '[' typeQualifierList? assignmentExpression? ']'
    |   directDeclarator '[' 'static' typeQualifierList? assignmentExpression ']'
    |   directDeclarator '[' typeQualifierList 'static' assignmentExpression ']'
    |   directDeclarator '[' typeQualifierList? '*' ']'
    |   directDeclarator '(' parameterTypeList ')'
    |   directDeclarator '(' identifierList? ')'
    ;

gccDeclaratorExtension
    :   '__asm' '(' StringLiteral+ ')'
    |   gccAttributeSpecifier
    ;

gccAttributeSpecifier
    :   '__attribute__' '(' '(' gccAttributeList ')' ')'
    ;

gccAttributeList
    :   gccAttributeListNonEmpty
    |   // empty
    ;

gccAttributeListNonEmpty
    :   gccAttributeListNonEmpty ',' gccAttribute
    |   gccAttribute
    ;

gccAttribute
    :   ~(',' | '(' | ')') // relaxed def for "identifier or reserved word"
        parenthesizedArgumentExpressionList?
    |   // empty
    ;

parenthesizedArgumentExpressionList : '(' argumentExpressionList? ')' ;

pointer
    :   '*' typeQualifierList?
    |   '*' typeQualifierList? pointer
    // ** is tokenized separately as separating conjunction
    |   '**' typeQualifierList?
    |   '**' typeQualifierList? pointer
    |   '^' typeQualifierList? // Blocks language extension
    |   '^' typeQualifierList? pointer // Blocks language extension
    ;

typeQualifierList
    :   typeQualifier
    |   typeQualifierList typeQualifier
    ;

parameterTypeList
    :   parameterList
    |   parameterList ',' '...'
    ;

parameterList
    :   parameterDeclaration
    |   parameterList ',' parameterDeclaration
    ;

parameterDeclaration
    :   declarationSpecifiers declarator
    |   declarationSpecifiers2 abstractDeclarator?
    ;

identifierList
    :   clangIdentifier
    |   identifierList ',' clangIdentifier
    ;

typeName
    :   specifierQualifierList abstractDeclarator?
    ;

abstractDeclarator
    :   pointer
    |   pointer? directAbstractDeclarator gccDeclaratorExtension*
    ;

directAbstractDeclarator
    :   '(' abstractDeclarator ')' gccDeclaratorExtension*
    |   '[' typeQualifierList? assignmentExpression? ']'
    |   '[' 'static' typeQualifierList? assignmentExpression ']'
    |   '[' typeQualifierList 'static' assignmentExpression ']'
    |   '[' '*' ']'
    |   '(' parameterTypeList? ')' gccDeclaratorExtension*
    |   directAbstractDeclarator '[' typeQualifierList? assignmentExpression? ']'
    |   directAbstractDeclarator '[' 'static' typeQualifierList? assignmentExpression ']'
    |   directAbstractDeclarator '[' typeQualifierList 'static' assignmentExpression ']'
    |   directAbstractDeclarator '[' '*' ']'
    |   directAbstractDeclarator '(' parameterTypeList? ')' gccDeclaratorExtension*
    ;

typedefName
    :   clangIdentifier
    ;

initializer
    :   '{' initializerList '}'
    |   '{' initializerList ',' '}'
    |   assignmentExpression
    ;

initializerList
    :   designation? initializer
    |   initializerList ',' designation? initializer
    ;

designation
    :   designatorList '='
    ;

designatorList
    :   designator
    |   designatorList designator
    ;

designator
    :   '[' constantExpression ']'
    |   '.' clangIdentifier
    ;

staticAssertDeclaration
    :   '_Static_assert' '(' constantExpression ',' StringLiteral+ ')' ';'
    ;

statement
    :   labeledStatement
    |   compoundStatement
    |   expressionStatement
    |   selectionStatement
    |   iterationStatement
    |   jumpStatement
    |   ('__asm' | '__asm__') ('volatile' | '__volatile__') '(' logicalOrExpressionList? logicalOrExpressionListColonList ')' ';'
    ;

logicalOrExpressionListColonList
    :   ':' logicalOrExpressionList? logicalOrExpressionListColonList
    |   // empty
    ;

logicalOrExpressionList
    :   logicalOrExpression
    |   logicalOrExpressionList ',' logicalOrExpression
    ;

labeledStatement
    :   clangIdentifier ':' statement
    |   'case' constantExpression ':' statement
    |   'default' ':' statement
    ;

compoundStatement
    :   '{' blockItemList? '}'
    |   ompBlockPragma '{' valEmbedContract? blockItemList? '}'
    ;

blockItemList
    :   blockItem
    |   blockItemList blockItem
    ;

blockItem
    :   declaration
    |   statement
    |   valEmbedStatementBlock
    |   {specLevel>0}? valStatement
    |   gpgpuBarrier
    |   gpgpuAtomicBlock
    ;

expressionStatement
    :   expression? ';'
    ;

selectionStatement
    :   'if' '(' expression ')' statement elseBranch?
    |   'switch' '(' expression ')' statement
    ;

elseBranch: 'else' statement;

iterationStatement
    :   valEmbedContract? 'while' '(' expression ')' valEmbedContract? statement
    |   'do' statement 'while' '(' expression ')' ';'
    |   valEmbedContract? ompLoopPragma? 'for' '(' expression? ';' expression? ';' expression? ')' valEmbedContract? statement
    |   valEmbedContract? ompLoopPragma? 'for' '(' declaration expression? ';' expression? ')' valEmbedContract? statement
    ;

jumpStatement
    :   'goto' clangIdentifier ';'
    |   'continue' ';'
    |   'break' ';'
    |   'return' expression? ';'
    |   'goto' unaryExpression ';' // GCC extension
    ;

compilationUnit
    :   translationUnit? EOF
    ;

translationUnit
    :   externalDeclaration
    |   translationUnit externalDeclaration
    ;

externalDeclaration
    :   functionDefinition
    |   declaration
    |   valEmbedGlobalDeclarationBlock
    |   ';' // stray ;
    ;

specificationDeclaration : Placeholder ;

/* The declarationSpecifiers contain the base type, declarator contains a pointer?, identifier and parameters and
 * compoundStatement is the function body. declarationList is used only in this syntax, which is unsupported, but valid.
 * int add1(arg)
 * int arg;
 * { return arg + 1; }
 */
functionDefinition
    :   valEmbedContract? declarationSpecifiers declarator declarationList? compoundStatement
    ;


declarationList
    :   declaration
    |   declarationList declaration
    ;

clangIdentifier
    :   Identifier
    |   valIdentifier
    ;