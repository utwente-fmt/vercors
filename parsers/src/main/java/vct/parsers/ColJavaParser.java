

package vct.parsers;

import static hre.lang.System.*;

import java.io.*;
import java.lang.reflect.Field;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.HashMap;
import java.util.Map;

import com.thoughtworks.xstream.io.binary.BinaryStreamDriver;
import hre.tools.TimeKeeper;
import org.antlr.v4.runtime.*;

import org.antlr.v4.runtime.atn.PredictionContext;
import org.antlr.v4.runtime.atn.PredictionContextCache;
import org.antlr.v4.runtime.atn.PredictionMode;
import org.antlr.v4.runtime.misc.ParseCancellationException;
import vct.antlr4.generated.*;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.parsers.rewrite.*;
import vct.col.ast.syntax.JavaDialect;
import vct.col.ast.syntax.JavaSyntax;

import com.thoughtworks.xstream.*;

import java.time.*;

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
        Field f = JavaParser.class.getDeclaredField("_sharedContextCache");
        Class<?> t = f.getType();
        f.setAccessible(true);
        PredictionContextCache pcc = (PredictionContextCache) f.get(null);
        Field cacheField = PredictionContextCache.class.getDeclaredField("cache");
        cacheField.setAccessible(true);

        File javaPCCStore = Paths.get("/tmp/javaPCCStore.tmp").toFile();
//        XStream xstream = new XStream(new BinaryStreamDriver());
        XStream xstream = new XStream();
        XStream.setupDefaultSecurity(xstream);
        xstream.allowTypesByWildcard(new String[] {
            "vct.**", "org.antlr.**"
        });
        if (javaPCCStore.exists() && javaPCCStore.isFile()) {
          Instant start = Instant.now();
          HashMap<PredictionContext, PredictionContext> mppCached = (HashMap<PredictionContext, PredictionContext>) xstream.fromXML(javaPCCStore);
          Integer globalNodeCountCached = (Integer) xstream.fromXML(Paths.get("/tmp/globalNodeCount.tmp").toFile());
          Instant end = Instant.now();

          cacheField.set(pcc, mppCached);

          Output("PredictionContext.globalNodeCount before: %d", PredictionContext.globalNodeCount);
          PredictionContext.globalNodeCount = Math.max(globalNodeCountCached, PredictionContext.globalNodeCount);
          Output("PredictionContext.globalNodeCount after: %d", PredictionContext.globalNodeCount);


          Output("Restored %d entries from cache, took %dms", mppCached.size(), Duration.between(start, end).toMillis());
        }

        TimeKeeper tk=new TimeKeeper();

        ProgramUnit pu;
        Lexer lexer = new LangJavaLexer(input);
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        JavaParser parser = new JavaParser(tokens);
        ErrorCounter ec = errorCounter(parser, lexer, file_name);
        if(this.topLevelSpecs) {
          parser.specLevel = 1;
        }

        JavaParser.CompilationUnitContext tree = parser.compilationUnit();
        ec.report();
        Progress("first parsing pass took %dms",tk.show());

        HashMap<PredictionContext, PredictionContext> mpp = (HashMap<PredictionContext, PredictionContext>) cacheField.get(pcc);
        FileOutputStream fileOut = new FileOutputStream("/tmp/javaPCCStore.tmp");
        Instant start = Instant.now();
        xstream.toXML(mpp, fileOut);
        fileOut.close();

        fileOut = new FileOutputStream("/tmp/globalNodeCount.tmp");
        xstream.toXML((Integer) PredictionContext.globalNodeCount, fileOut);
        fileOut.close();

        Instant end = Instant.now();
        Output("PredictionContext.globalNodeCount finally: %d", PredictionContext.globalNodeCount);
        Output("Saved %d cached entries, took %dms", mpp.size(), Duration.between(start, end).toMillis());

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

