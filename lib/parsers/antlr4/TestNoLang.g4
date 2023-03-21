parser grammar TestNoLang;
options { tokenVocab = SpecLexer; }
import SpecParser;

/* This file is the opposite of TestNoSpec{Lexer,Parser}. Here we want to test whether the lexer of the specification
 * parser is complete */

langEmp: EOF EOF;
langExpr: langEmp;
langId: langEmp;
langType: langEmp;
langModifier: langEmp;
langStatement: langEmp;
startSpec: langEmp;
endSpec: langEmp;