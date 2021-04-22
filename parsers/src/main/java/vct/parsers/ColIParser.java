package vct.parsers;

import hre.tools.TimeKeeper;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import scala.collection.Seq;
import vct.antlr4.generated.CParser;
import vct.antlr4.generated.LangCLexer;
import vct.col.ast.GlobalDeclaration;
import vct.col.ast.Program;
import vct.col.ast.stmt.decl.ProgramUnit;

import java.nio.file.Paths;

import static hre.lang.System.Debug;
import static hre.lang.System.Progress;
import static hre.lang.System.Output;

/**
 * Parse specified code and convert the contents to COL. 
 */
public class ColIParser extends Parser {
  @Override
  public ProgramUnit parse(CharStream input, String file_name) {
    TimeKeeper tk=new TimeKeeper();

    LangCLexer lexer = new LangCLexer(input);
    CommonTokenStream tokens = new CommonTokenStream(lexer);
    CParser parser = new CParser(tokens);
    ErrorCounter ec = errorCounter(parser, lexer, file_name);

    CParser.CompilationUnitContext tree = parser.compilationUnit();
    Progress("first parsing pass took %dms",tk.show());
    ec.report();
    Debug("parser got: %s",tree.toStringTree(parser));

    InterpretedFileOriginProvider originProvider =
            new InterpretedFileOriginProvider(tokens, Paths.get("examples/basic/pointer.c"), Paths.get(file_name));
    Seq<GlobalDeclaration> pu = new CToCol(originProvider, originProvider).convert(tree);
    Progress("AST conversion took %dms",tk.show());
    Debug("after conversion %s",pu);

    Output("%s", new ParseError(pu.apply(0).subnodes().apply(2).subnodes().apply(1).o(), "hello, world!").text());

    Output("%s", pu);

    // TODO: consider restoring comparision chaining (a<b<c<d) and range perms (Perm(a[{0..n}], write))
    
    return null;
  }
}

