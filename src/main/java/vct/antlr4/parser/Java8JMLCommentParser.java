package vct.antlr4.parser;

import hre.tools.TimeKeeper;
import org.antlr.v4.runtime.tree.ParseTree;

import vct.col.ast.generic.ASTSequence;
import vct.antlr4.generated.Java8JMLLexer;
import vct.antlr4.generated.Java8JMLParser;

/**
 * Parser for JML comments.
 */
public class Java8JMLCommentParser extends CommentParser<Java8JMLParser,Java8JMLLexer> {

  
  public Java8JMLCommentParser(ErrorCounter ec) {
    super(ec,new Java8JMLParser(null), new Java8JMLLexer(null));
  }

  @Override
  public TempSequence parse_contract(ASTSequence<?> seq) {
    TimeKeeper tk = new TimeKeeper();
    ParseTree tree=parser.specificationSequence();
    long middle = tk.show();
    ec.report();
    TempSequence res=Java8JMLtoCol.convert_seq(tree, "embedded_comments", tokens, parser);
    long end = tk.show();
    hre.lang.System.Progress("comment parsing/conversion %d/%d", middle, end);
    return res;
  }

  @Override
  public TempSequence parse_annotations() {
  	ParseTree tree=parser.extraAnnotation();
  	ec.report();
  	TempSequence res=Java8JMLtoCol.convert_seq(tree, "embedded_comments", tokens, parser);
  	return res;
  }

}
