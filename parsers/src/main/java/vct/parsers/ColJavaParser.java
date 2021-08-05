

package vct.parsers;

import static hre.lang.System.*;

import hre.tools.TimeKeeper;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;

import vct.antlr4.generated.*;
import vct.col.ast.stmt.decl.ProgramUnit;

/**
 * Parse specified code and convert the contents to COL. 
 */
public class ColJavaParser extends Parser {
  public final boolean topLevelSpecs;
  
  public ColJavaParser(boolean topLevelSpecs){
    this.topLevelSpecs = topLevelSpecs;
  }
  
  @Override
  public ProgramUnit parse(CharStream input, String fileName) {
      try {
        TimeKeeper tk=new TimeKeeper();

        Lexer lexer = new LangJavaLexer(input);
        CommonTokenStream tokenStream = new CommonTokenStream(lexer);
        JavaParser parser = new JavaParser(tokenStream);
        ErrorCounter ec = errorCounter(parser, lexer, fileName);

        JavaParser.CompilationUnitContext tree = (JavaParser.CompilationUnitContext) parseLLSLL(
                ec,
                parser,
                () -> {
                  // Parser might be run twice in case of SLL errors, so specLevel must definitely be reset.
                  // Otherwise it might remain "1" because of the previous run accidentally
                  if (this.topLevelSpecs) {
                    parser.specLevel = 1;
                  } else {
                    parser.specLevel = 0;
                  }

                  return parser.compilationUnit();
                },
                false
        );

        ec.report();
        Progress("parsing pass took %dms",tk.show());

        ProgramUnit pu=JavaJMLtoCOL.convert(tree,fileName,tokenStream,parser);
        Progress("AST conversion pass took %dms",tk.show());
        Debug("program after Java parsing:%n%s",pu);

        return pu;
      } catch (Exception e) {
        DebugException(e);
        Abort("Exception %s while parsing %s",e.getClass(),fileName);
      } catch (Throwable e){
        DebugException(e);
        Warning("Exception %s while parsing %s",e.getClass(),fileName);
        throw e;
      }
    return null;
  }

}

