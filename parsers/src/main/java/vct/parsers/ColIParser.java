package vct.parsers;

import hre.tools.TimeKeeper;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import vct.antlr4.generated.LangCLexer;
import vct.antlr4.generated.CParser;
import vct.col.ast.stmt.decl.ProgramUnit;

import static hre.lang.System.*;

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
    Progress("First parsing pass took %dms",tk.show());
    ec.report();
    Debug("parser got: %s",tree.toStringTree(parser));

    ProgramUnit pu= CMLtoCOL.convert(tree,file_name,tokens,parser);
    pu.setLanguageFlag(ProgramUnit.LanguageFlag.SeparateArrayLocations, false);
    Progress("AST conversion took %dms",tk.show());
    Debug("after conversion %s",pu);

    // TODO: consider restoring comparision chaining (a<b<c<d) and range perms (Perm(a[{0..n}], write))
    
    return pu;
  }
}

