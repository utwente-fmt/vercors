package vct.parsers;

import hre.tools.TimeKeeper;
import org.antlr.v4.runtime.CharStream;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import vct.antlr4.generated.LangCLexer;
import vct.antlr4.generated.CParser;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.parsers.rewrite.*;
import vct.col.ast.syntax.CSyntax;

import java.io.*;

import static hre.lang.System.*;

/**
 * Parse specified code and convert the contents to COL. 
 */
public class ColIParser implements Parser {

  protected ProgramUnit parse(String file_name,InputStream stream) throws IOException{
    TimeKeeper tk=new TimeKeeper();
    ErrorCounter ec=new ErrorCounter(file_name);

    CharStream input = CharStreams.fromStream(stream);
    LangCLexer lexer = new LangCLexer(input);
    lexer.removeErrorListeners();
    lexer.addErrorListener(ec);
    CommonTokenStream tokens = new CommonTokenStream(lexer);
    CParser parser = new CParser(tokens);
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
    
    pu=new FlattenVariableDeclarations(pu).rewriteAll();
    Progress("Flattening variables took %dms",tk.show());
    Debug("after flattening variable decls %s",pu);
    
    pu=new SpecificationCollector(CSyntax.getCML(),pu).rewriteAll();
    Progress("Shuffling specifications took %dms",tk.show());    
    Debug("after collecting specifications %s",pu);

    pu = new RewriteWithThen(pu).rewriteAll();
    Progress("rewriting with/then blocks took %dms", tk.show());

    pu = new AnnotationInterpreter(pu).rewriteAll();
    Progress("rewriting extra annotations took %dms", tk.show());

    pu=new ConvertTypeExpressions(pu).rewriteAll();
    Progress("converting type expressions took %dms",tk.show());
    Debug("after converting type expression %s",pu);

    pu = new KernelInvocationToMethodInvocation(pu).rewriteAll();
        
    pu=new VerCorsDesugar(pu).rewriteAll();
    Progress("Desugaring took %dms",tk.show());
    Debug("after desugaring",pu);

    // TODO: encoding as class should not be necessary. 
    pu=new EncodeAsClass(pu).rewriteAll();
    Progress("Encoding as class took %dms",tk.show());
    Debug("after encoding as class %s",pu);
    
    pu=new FilterSpecIgnore(pu).rewriteAll();
    pu=new StripUnusedExtern(pu).rewriteAll();
    Progress("Stripping unused parts took %dms",tk.show());    
    Debug("after stripping unused parts %s",pu);
    
    return pu;
  }
  
  @Override
  public ProgramUnit parse(File file) {
    String file_name=file.toString();
    try {
      InputStream stream =new FileInputStream(file);
      return parse(file_name,stream);
    } catch (FileNotFoundException e) {
      Fail("File %s has not been found",file_name);
    } catch (Exception e) {
      DebugException(e);
      Abort("Exception %s while parsing %s",e.getClass(),file_name);
    }
    return null;
  }

}

