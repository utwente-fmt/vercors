package vct.silver;

import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.rewrite.SatCheckRewriter;
import viper.api.*;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;

import hre.ast.Origin;
import hre.config.IntegerSetting;
import hre.config.StringSetting;
import hre.io.Container;
import hre.io.JarContainer;
import hre.lang.HREError;
import hre.lang.HREException;
import hre.util.ContainerClassLoader;
import vct.error.VerificationError;
import vct.logging.MessageFactory;
import vct.logging.PassAddVisitor;
import vct.logging.PassReport;
import vct.logging.TaskBegin;
import vct.util.Configuration;

import static hre.lang.System.DebugException;
import static hre.lang.System.Output;
import static hre.lang.System.Warning;

public class SilverBackend {
  
  public static StringSetting silver_module=new StringSetting(null);
  public static IntegerSetting silicon_z3_timeout=new IntegerSetting(30000);
  
  public static <T,E,S,DFunc,DAxiom,Program>
  ViperAPI<Origin,VerificationError,T,E,S,DFunc,DAxiom,Program>
  getVerifier(String tool){
    if (silver_module.used()){  
      return getSilverModuleVerifier(tool);
    } else {
      return getViperVerifier(tool);
    }
  }
  
  public static <T,E,S,DFunc,DAxiom,Program>
  ViperAPI<Origin,VerificationError,T,E,S,DFunc,DAxiom,Program>
  getSilverModuleVerifier(String tool){
    return null;
    // boolean parser=tool.equals("parser");
    // if (parser){
    //   tool="silicon";
    // }
    // File jarfile;
    // jarfile=Configuration.getToolHome().resolve(silver_module.get()+"/"+tool+".jar").toFile();
    // Container container;

    //   container=new JarContainer(jarfile);
    // Object obj;
    // //TODO: Properties silver_props=new Properties();
    // //TODO: Properties verifier_props=new Properties();
    // try {
    //   ClassLoader loader=new ContainerClassLoader(container);
    //   //TODO: InputStream is=loader.getResourceAsStream("silver.hglog");
    //   //TODO: silver_props.load(is);
    //   //TODO: is.close();
    //   //TODO: is=loader.getResourceAsStream("verifier.hglog");
    //   //TODO: verifier_props.load(is);
    //   //TODO: is.close();
    //   Class<?> v_class;
    //   if (parser) {
    //     v_class=loader.loadClass("viper.api.SilverImplementation");
    //   } else if (tool.contains("silicon")){
    //     v_class=loader.loadClass("viper.api.SiliconVerifier");
    //   } else if (tool.contains("carbon")) {
    //     v_class=loader.loadClass("viper.api.CarbonVerifier");
    //   } else {
    //     throw new HREError("cannot guess the main class of %s",tool);
    //   }
    //   Constructor<?>[] constructors = v_class.getConstructors();
    //   if (constructors.length!=1) {
    //     throw new HREError("class had %d constructors instead of 1",constructors.length);
    //   }
    //   obj=constructors[0].newInstance(new HREOrigins());
    // } catch(Exception e) {
    //   DebugException(e);
    //   throw new HREError("Exception %s",e);
    // }
    // if (!(obj instanceof ViperAPI)){
    //   hre.lang.System.Fail("Plugin is incompatible: cannot cast verifier.");
    // }
    // @SuppressWarnings("unchecked")
    // ViperAPI<Origin,VerificationError,T,E,S,DFunc,DAxiom,Program> verifier=(ViperAPI<Origin, VerificationError, T, E, S, DFunc, DAxiom, Program>)obj;
    // //verifier.set_tool_home(Configuration.getToolHome());
    // return verifier;
  }
  
  public static <T,E,S,DFunc,DAxiom,Program>
  ViperAPI<Origin,VerificationError,T,E,S,DFunc,DAxiom,Program>
  getViperVerifier(String tool){
    ViperAPI<Origin,VerificationError,T,E,S,DFunc,DAxiom,Program> verifier = null;  
    switch(tool.trim()) {
    case "carbon":
      verifier = new CarbonVerifier(new HREOrigins());
      break;
    case "silicon":
      verifier = new SiliconVerifier(new HREOrigins());
      break;
    case "parser":
      verifier = new SilverImplementation(new HREOrigins());
      break;
    default:
      throw new HREError("cannot guess the main class of %s",tool);    
    }
    return verifier;
  }
  
  public static <T,E,S,Decl,DFunc,DAxiom,Program>
  PassReport TestSilicon(PassReport given, String tool) {
    //hre.System.Output("verifying with %s backend",silver_module.get());
    ProgramUnit arg=given.getOutput();
    PassReport report=new PassReport(arg);
    report.add(new PassAddVisitor(given));
    MessageFactory log=new MessageFactory(new PassAddVisitor(report));
    TaskBegin verification=log.begin("Viper verification");
    ViperAPI<Origin,VerificationError,T,E,S,DFunc,DAxiom,Program> verifier=getVerifier(tool);
    hre.lang.System.Progress("verifying with %s %s backend",
            silver_module.used()?silver_module.get():"builtin",tool);
    //verifier.set_detail(Configuration.detailed_errors.get());
    VerCorsViperAPI vercors=VerCorsViperAPI.get();
    Program program=vercors.prog.convert(verifier,arg);
    log.phase(verification,"Backend AST conversion");
    String fname=vct.util.Configuration.backend_file.get();
    if (fname!=null){
      PrintWriter pw=null;
      try {
         pw = new java.io.PrintWriter(new java.io.File(fname));
         verifier.write_program(pw,program);
      } catch (FileNotFoundException e) {
        DebugException(e);
      } finally {
        if (pw!=null) pw.close();
      }
    }

    Properties settings=new Properties();
    if (tool.startsWith("silicon")){
      //settings.setProperty("smt.soft_timeout",silicon_z3_timeout.get()+"");
    }
    ViperControl control=new ViperControl(log);
    try {
      // Call into Viper to verify!
      List<? extends ViperError<Origin>> rawErrors = verifier.verify(
              Configuration.getZ3Path().toPath(),
              settings,
              program,
              control
      );
      // Put it in a new list so we can add ViperErrors to it ourselves
      List<ViperError<Origin>> errors = new ArrayList<>(rawErrors);

      // Filter SatCheck errors that are to be expected
      HashSet<SatCheckRewriter.AssertOrigin> satCheckAssertsSeen = new HashSet<>();
      errors.removeIf(e -> {
        for (int i = 0; i < e.getExtraCount(); i++) {
          Origin origin = e.getOrigin(i);
          if (origin instanceof SatCheckRewriter.AssertOrigin) {
            satCheckAssertsSeen.add((SatCheckRewriter.AssertOrigin) origin);
            return true;
          }
        }
        return false;
      });

      // For each satCheckAssert that did not error, it means the contract was requires false; or something similar
      // Therefore, we warn the user that their contracts are unsound
      HashSet<Origin> expectedSatCheckAsserts = vercors.getSatCheckAsserts();
      for (Origin expectedSatCheckAssert : expectedSatCheckAsserts) {
        if (!satCheckAssertsSeen.contains(expectedSatCheckAssert)) {
          ViperErrorImpl<Origin> error = new ViperErrorImpl<Origin>(expectedSatCheckAssert, "method.precondition.unsound:method.precondition.false");
          errors.add(error);
        }
      }

      if (errors.size() == 0) {
        Output("Success!");
      } else {
        Output("Errors! (%d)", errors.size());
        for(ViperError<Origin> e:errors){
          log.error(e);
        }
      }
    } catch (Exception e){
      log.exception(e);
    } finally {
      control.done();
    }
    log.end(verification);
    return report;
  }

}
