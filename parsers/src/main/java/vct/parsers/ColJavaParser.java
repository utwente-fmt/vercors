

package vct.parsers;

import static hre.lang.System.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

import hre.tools.TimeKeeper;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;

import vct.antlr4.generated.*;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.parsers.rewrite.*;
import vct.col.ast.syntax.JavaDialect;
import vct.col.ast.syntax.JavaSyntax;

/**
 * Parse specified code and convert the contents to COL. 
 */
public class ColJavaParser extends Parser {
  public final boolean topLevelSpecs;
  
  public ColJavaParser(boolean topLevelSpecs){
    this.topLevelSpecs = topLevelSpecs;
  }
  
  @Override
  public ProgramUnit parse(CharStream input, String file_name) {
      try {
        TimeKeeper tk=new TimeKeeper();

        ProgramUnit pu;
        ErrorCounter ec=new ErrorCounter(file_name);
        Lexer lexer = new LangJavaLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        JavaParser parser = new JavaParser(tokens);
        parser.removeErrorListeners();
        parser.addErrorListener(ec);
        lexer.removeErrorListeners();
        lexer.addErrorListener(ec);
        if(this.topLevelSpecs) {
          parser.specLevel = 1;
        }

        JavaParser.CompilationUnitContext tree = parser.compilationUnit();
        ec.report();
        Progress("first parsing pass took %dms",tk.show());

        pu=JavaJMLtoCOL.convert(tree,file_name,tokens,parser);
        Progress("AST conversion took %dms",tk.show());
        Debug("program after Java parsing:%n%s",pu);

        return pu;
      } catch (Exception e) {
        DebugException(e);
        Abort("Exception %s while parsing %s",e.getClass(),file_name);
      } catch (Throwable e){
        DebugException(e);
        Warning("Exception %s while parsing %s",e.getClass(),file_name);
        throw e;
      }
    return null;
  }

}

