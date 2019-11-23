package vct.antlr4.parser;

import static hre.lang.System.*;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;

import hre.tools.TimeKeeper;
import org.antlr.v4.runtime.ANTLRInputStream;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;

import vct.antlr4.generated.*;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.rewrite.ConvertTypeExpressions;
import vct.col.rewrite.EncodeAsClass;
import vct.col.rewrite.FilterSpecIgnore;
import vct.col.rewrite.FlattenVariableDeclarations;
import vct.col.rewrite.StripUnusedExtern;
import vct.col.rewrite.VerCorsDesugar;
import vct.col.syntax.CSyntax;

/**
 * Parse specified code and convert the contents to COL. 
 */
public class ColIParser implements vct.col.util.Parser {

  protected ProgramUnit parse(String file_name,InputStream stream) throws IOException{
    TimeKeeper tk=new TimeKeeper();
    ErrorCounter ec=new ErrorCounter(file_name);

    ANTLRInputStream input = new ANTLRInputStream(stream);
    CMLLexer lexer = new CMLLexer(input);
    lexer.removeErrorListeners();
    lexer.addErrorListener(ec);
    CommonTokenStream tokens = new CommonTokenStream(lexer);
    CMLParser parser = new CMLParser(tokens);
    parser.removeErrorListeners();
    parser.addErrorListener(ec);
    ParseTree tree = parser.compilationUnit();
    Progress("first parsing pass took %dms",tk.show());
    ec.report();
    Debug("parser got: %s",tree.toStringTree(parser));

    ProgramUnit pu=CMLtoCOL.convert_pu(tree,file_name,tokens,parser);
    pu.setLanguageFlag(ProgramUnit.LanguageFlag.SeparateArrayLocations, false);
    Progress("AST conversion took %dms",tk.show());
    Debug("after conversion %s",pu);
    
    pu=new CommentRewriter(pu,new CMLCommentParser(ec)).rewriteAll();
    Progress("Specification parsing took %dms",tk.show());
    ec.report();
    Debug("after comment processing %s",pu);

    pu=new FlattenVariableDeclarations(pu).rewriteAll();
    Progress("Flattening variables took %dms",tk.show());
    Debug("after flattening variable decls %s",pu);
    
    pu=new SpecificationCollector(CSyntax.getCML(),pu).rewriteAll();
    Progress("Shuffling specifications took %dms",tk.show());    
    Debug("after collecting specifications %s",pu);

    pu=new ConvertTypeExpressions(pu).rewriteAll();
    Progress("converting type expressions took %dms",tk.show());
    Debug("after converting type expression %s",pu);
        
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

