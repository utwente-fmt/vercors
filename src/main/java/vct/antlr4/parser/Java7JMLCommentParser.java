package vct.antlr4.parser;

import org.antlr.v4.runtime.tree.ParseTree;

import vct.col.ast.generic.ASTSequence;
import vct.antlr4.generated.Java7JMLLexer;
import vct.antlr4.generated.Java7JMLParser;

/**
 * Parser for JML comments.
 */
public class Java7JMLCommentParser extends CommentParser<Java7JMLParser,Java7JMLLexer> {

  
  public Java7JMLCommentParser(ErrorCounter ec) {
    super(ec,new Java7JMLParser(null), new Java7JMLLexer(null));
  }

  @Override
  public TempSequence parse_contract(ASTSequence<?> seq) {
    ParseTree tree=parser.specificationSequence();
    ec.report();
    return Java7JMLtoCol.convert_seq(tree, "embedded_comments", tokens, parser);
  }

  @Override
  public TempSequence parse_annotations() {
	ParseTree tree=parser.extraAnnotation();
	ec.report();
	return Java7JMLtoCol.convert_seq(tree, "embedded_comments", tokens, parser);
  }

}
