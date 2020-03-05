

package vct.antlr4.parser;

import static hre.lang.System.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;

import hre.tools.TimeKeeper;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.Lexer;
import org.antlr.v4.runtime.tree.ParseTree;

import vct.antlr4.generated.*;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.rewrite.AnnotationInterpreter;
import vct.col.rewrite.FilterSpecIgnore;
import vct.col.rewrite.FlattenVariableDeclarations;
import vct.col.syntax.JavaDialect;
import vct.col.syntax.JavaSyntax;

/**
 * Parse specified code and convert the contents to COL. 
 */
public class ColJavaParser implements vct.col.util.Parser {

  public final int version;
  public final boolean twopass;
  public final boolean topLevelSpecs;
  
  public ColJavaParser(int version, boolean twopass, boolean topLevelSpecs){
    this.version=version;
    this.twopass=twopass;
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
        
        switch(version){
        case 7:
          if (twopass){
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
            break;
          } else {
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
            break;
          }
        default:
          throw new Error("bad java version: "+version);
        }
        pu=new FlattenVariableDeclarations(pu).rewriteAll();
        Progress("Flattening variables took %dms",tk.show());
        Debug("program after flattening variables:%n%s",pu);
        
        pu=new SpecificationCollector(JavaSyntax.getJava(JavaDialect.JavaVerCors),pu).rewriteAll();
        Progress("Shuffling specifications took %dms",tk.show());        
        Debug("program after collecting specs:%n%s",pu);
        
        pu=new JavaPostProcessor(pu).rewriteAll();
        Progress("post processing took %dms",tk.show());        

        pu=new AnnotationInterpreter(pu).rewriteAll();
        Progress("interpreting annotations took %dms",tk.show());        

        //cannnot resolve here: other .java files may be needed!
        //pu=new JavaResolver(pu).rewriteAll();
        //Progress("resolving library calls took %dms",tk.show());        

        pu=new FilterSpecIgnore(pu).rewriteAll();
        Progress("filtering spec_ignore took %dms",tk.show()); 

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

