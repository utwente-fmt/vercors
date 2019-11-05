/**
  Interface to ANTLR4 parsers.

  The entry point for using this package is the <code>Parser</code> class.
  The <code>parse</code> method in this class will select a parser based
  on the extension of the file name and call the appropriate ANTLR4 parser.
  Next it will convert that ANTLR4 parse tree to a COL abstract syntax tree.
  For languages that do not have a built-in specification language,
  the next step is to scan the syntax tree for specification comments,
  to parse those comments, convert them to COL and attach them in the
  main syntax tree.
  Finally, for some languages (E.g. Java) there is a post-processor 
  that has to be run to produce the correct AST. 
  
  The architecture for the conversion of an ANTLR parse tree to a COL
  AST for the various languages is based on three levels. At the top
  level, we have the <code>ANTLRtoCOL</code> class that provides
  conversion for common features, such as sequences of declarations
  and expressions. The middle level is the one shared by the specification
  language and the programming languages. When using a separate grammar
  for the specification language large part of the grammar are shared.
  For example JML allows the same expressions as Java. The middle
  level allow sharing the implementation of the parse tree conversion.
  The lowest level is specific to each language.
  
  The three layers are implemented with inheritance. The base class
  is ANTLRtoCOL and implements ParseTreeVisitor, which makes it
  a visitor for arbitrary ANTLR parse trees. The middle level
  object extend this class with functions needed for a family of
  languages. The language specific converter classes extend the 
  language family class and implement the generated language specific
  visitor class.
  
  To keep the implementation effort of the language specific visitor
  down to a minimum, the code has been designed in such a way that
  control flow starts with the base class, which calls the visitor
  to check if the conversion is language specific (non-null return
  value) or if it should use generic code (null return value).
  
  
 @author <a href="mailto:s.c.c.blom@utwente.nl">Stefan Blom</a>
 */
package vct.antlr4.parser;

