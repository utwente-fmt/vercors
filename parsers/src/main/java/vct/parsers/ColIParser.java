package vct.parsers;

import hre.tools.TimeKeeper;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import vct.antlr4.generated.LangCLexer;
import vct.antlr4.generated.CParser;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.syntax.JavaDialect;
import vct.col.ast.syntax.JavaSyntax;
import vct.parsers.rewrite.*;
import vct.col.ast.syntax.CSyntax;

import java.io.*;

import static hre.lang.System.*;

/**
 * Parse specified code and convert the contents to COL. 
 */
public class ColIParser extends Parser {
  @Override
  public ProgramUnit parse(CharStream input, String file_name) {
    TimeKeeper tk=new TimeKeeper();
    ErrorCounter ec=new ErrorCounter(file_name);

    LangCLexer lexer = new LangCLexer(input);
    lexer.removeErrorListeners();
    lexer.addErrorListener(ec);
    CommonTokenStream tokens = new CommonTokenStream(lexer);
    CParser parser = new CParser(tokens);
    parser.reset();
    parser.removeErrorListeners();
    parser.addErrorListener(ec);
    CParser.CompilationUnitContext tree = parser.compilationUnit();
    Progress("first parsing pass took %dms",tk.show());
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

