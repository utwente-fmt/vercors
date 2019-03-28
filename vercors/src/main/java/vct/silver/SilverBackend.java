package vct.silver;

import viper.api.*;

import java.io.File;
import java.io.FileNotFoundException;
import java.io.PrintWriter;
import java.lang.reflect.Constructor;
import java.util.HashSet;
import java.util.List;
import java.util.Properties;

import hre.ast.Origin;
import hre.config.IntegerSetting;
import hre.config.StringSetting;
import hre.io.Container;
import hre.io.DirContainer;
import hre.io.JarContainer;
import hre.io.UnionContainer;
import hre.lang.HREError;
import hre.lang.HREException;
import hre.util.ContainerClassLoader;
import vct.col.ast.*;
import vct.error.VerificationError;
import vct.logging.MessageFactory;
import vct.logging.PassAddVisitor;
import vct.logging.PassReport;
import vct.logging.TaskBegin;
import vct.util.Configuration;

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
    boolean parser=tool.equals("parser");
    if (parser){
      tool="silicon";
    }
    File jarfile;
    jarfile=Configuration.getToolHome().resolve(silver_module.get()+"/"+tool+".jar").toFile();
    //System.err.printf("adding jar %s to path%n",jarfile);
    Container container;

      container=new JarContainer(jarfile);
    Object obj;
    //TODO: Properties silver_props=new Properties();
    //TODO: Properties verifier_props=new Properties();
    try {
      ClassLoader loader=new ContainerClassLoader(container);
      //TODO: InputStream is=loader.getResourceAsStream("silver.hglog");
      //TODO: silver_props.load(is);
      //TODO: is.close();
      //TODO: System.err.printf("silver properties: %s%n", silver_props);
      //TODO: is=loader.getResourceAsStream("verifier.hglog");
      //TODO: verifier_props.load(is);
      //TODO: is.close();
      //TODO: System.err.printf("verifier properties: %s%n", verifier_props);
      Class<?> v_class;
      if (parser) {
        v_class=loader.loadClass("viper.api.SilverImplementation");
      } else if (tool.contains("silicon")){
        v_class=loader.loadClass("viper.api.SiliconVerifier");
      } else if (tool.contains("carbon")) {
        v_class=loader.loadClass("viper.api.CarbonVerifier");
      } else {
        throw new HREError("cannot guess the main class of %s",tool);
      }
      Constructor<?>[] constructors = v_class.getConstructors();
      if (constructors.length!=1) {
        throw new HREError("class had %d constructors instead of 1",constructors.length);
      }
      obj=constructors[0].newInstance(new HREOrigins());
    } catch(Exception e) {
      e.printStackTrace();
      throw new HREError("Exception %s",e);
    }
    if (!(obj instanceof ViperAPI)){
      hre.lang.System.Fail("Plugin is incompatible: cannot cast verifier.");
    }
    @SuppressWarnings("unchecked")
    ViperAPI<Origin,VerificationError,T,E,S,DFunc,DAxiom,Program> verifier=(ViperAPI<Origin, VerificationError, T, E, S, DFunc, DAxiom, Program>)obj;
    //verifier.set_tool_home(Configuration.getToolHome());
    return verifier;
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
    hre.lang.System.Output("verifying with %s %s backend",
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
        e.printStackTrace();
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
      HashSet<Origin> reachable=new HashSet<Origin>();
      List<? extends ViperError<Origin>> errs=verifier.verify(
          Configuration.getToolHome(),settings,program,reachable,control);
      if (errs.size()>0){
        for(ViperError<Origin> e:errs){
          log.error(e);
        }
      }
      HashSet<Origin> accounted=new HashSet<Origin>();
      Configuration.detailed_errors.get();
      for(String method:control.verified_methods){
        boolean pass=true;
        for(Origin o:vercors.refuted.get(method)){
          if(!reachable.contains(o)){
            log.exception(new HREException("%s: unreachable",o));
            pass=false;
          } else {
            accounted.add(o);
          }
        }
        System.err.printf("method verdict %s %s%n",method,pass?"PASS":"FAIL");
      }
      for(String method:control.failed_methods){
        System.err.printf("method verdict %s FAIL%n",method);
        for(Origin o:vercors.refuted.get(method)){
          accounted.add(o);
        }
      }
      /*
      System.err.printf("accounted: %n");
      for(Origin o:accounted){
        System.err.printf("  %s%n",o);
      }
      System.err.printf("reachable: %n");
      for(Origin o:reachable){
        System.err.printf("  %s%n",o);
      }
      */
      for(Origin o:reachable){
        if (!accounted.contains(o)){
          System.err.printf("unregistered location %s marked reachable%n",o);
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
