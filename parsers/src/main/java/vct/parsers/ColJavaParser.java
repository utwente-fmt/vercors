

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
public class ColJavaParser implements Parser {
  public final boolean topLevelSpecs;
  
  public ColJavaParser(boolean topLevelSpecs){
    this.topLevelSpecs = topLevelSpecs;
  }
  
  @Override
  public ProgramUnit parse(File file) {
    String file_name=file.toString();
      try {
        TimeKeeper tk=new TimeKeeper();
        
        CharStream input = CharStreams.fromStream(new FileInputStream(file));

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

        pu = new SpecificationCollector(JavaSyntax.getJava(JavaDialect.JavaVerCors), pu).rewriteAll();
        
        pu=new JavaPostProcessor(pu).rewriteAll();
        Progress("post processing took %dms",tk.show());        

        pu = new RewriteWithThen(pu).rewriteAll();
        Progress("rewriting with/then blocks took %dms", tk.show());

        pu=new AnnotationInterpreter(pu).rewriteAll();
        Progress("interpreting annotations took %dms",tk.show());

        return pu;
      } catch (FileNotFoundException e) {
        Fail("File %s has not been found",file_name);
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

