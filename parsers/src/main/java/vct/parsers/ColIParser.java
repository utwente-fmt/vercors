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
  public ProgramUnit parse(CharStream input, String fileName) {
    TimeKeeper tk=new TimeKeeper();

    LangCLexer lexer = new LangCLexer(input);
    CommonTokenStream tokens = new CommonTokenStream(lexer);
    CParser parser = new CParser(tokens);
    ErrorCounter ec = errorCounter(parser, lexer, fileName);

    CParser.CompilationUnitContext tree = (CParser.CompilationUnitContext) parseLLSLL(
            ec,
            parser,
            () -> {
              // Reset the specLevel in case the parser is reused for the second parse
              parser.specLevel = 0;
              return parser.compilationUnit();
            },
            false
    );
    Progress("first parsing pass took %dms",tk.show());
    ec.report();
    Debug("parser got: %s",tree.toStringTree(parser));

    ProgramUnit pu= CMLtoCOL.convert(tree,fileName,tokens,parser);
    pu.setLanguageFlag(ProgramUnit.LanguageFlag.SeparateArrayLocations, false);
    Progress("AST conversion took %dms",tk.show());
    Debug("after conversion %s",pu);

    // TODO: consider restoring comparision chaining (a<b<c<d) and range perms (Perm(a[{0..n}], write))
    
    return pu;
  }
}

