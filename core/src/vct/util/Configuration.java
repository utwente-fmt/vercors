package vct.util;

import static hre.System.Abort;
import static hre.System.Debug;
import static hre.System.Progress;
import static hre.System.Warning;

import java.io.File;
import java.io.IOException;
import java.net.URL;
import java.nio.file.Path;
import java.nio.file.Paths;

import vct.col.syntax.JavaSyntax;
import vct.col.syntax.Syntax;
import vct.java.printer.JavaDialect;
import hre.config.BooleanSetting;
import hre.config.OptionParser;
import hre.config.StringListSetting;
import hre.config.StringSetting;
import hre.io.Message;
import hre.io.ModuleShell;
/**
 * This class contains the configuration options of the VerCors library.
 * 
 * @author Stefan Blom
 *
 */
public class Configuration {

  /**
   * 
   */
  public static final StringListSetting modulepath;
  
  /**
   * Switch behavior of witness encoding.
   */
  public static final BooleanSetting witness_constructors=new BooleanSetting(true);
  
  /**
   * Global options for controlling the deletion of temporary files.
   */
  public static final BooleanSetting keep_temp_files=new BooleanSetting(false);
  /**
   * Global options for increasing the detail used in error messages.
   * The idea is that normal error messages are independent of the
   * back-end used, while detailed messages may contain details which
   * are specific to a back-end.
   */
  public static final BooleanSetting detailed_errors=new BooleanSetting(false);
  
  /**
   * Set the name of the file that is fed into the back-end verifier.
   * The file is kept after the verification.
   */
  public static final StringSetting backend_file=new StringSetting(null);
  
  /**
   * Control type checking in the PVL parser.
   * By default type checking is enabled, but for multiple file input
   * it must often be disabled as the PVL type checker does not consider libraries.
   */
  public static final BooleanSetting pvl_type_check=new BooleanSetting(true);
  
  /**
   * When a kernel is a single group, some important simplifications can be performed.
   * Thus we have this option that tells the tools to assume gsize==tcount.
   */
  public static final BooleanSetting assume_single_group=new BooleanSetting(false);
  
  /**
   * This setting which is true by default controls if all resource
   * are automatically revoked with every kernel barrier.
   */
  public static final BooleanSetting auto_barrier=new BooleanSetting(true);
  
  /**
   * Enable the resource check during kernel verification.
   */
  public static final BooleanSetting enable_resource_check=new BooleanSetting(true);
  
  /**
   * Enable post check during kernel verification. 
   */
  public static final BooleanSetting enable_post_check=new BooleanSetting(true);
  
  /**
   * The include path passed to the C pre processor.
   */
  public static final StringListSetting cpp_include_path=new StringListSetting();
  
  /**
   * The definitions passed to the C pre processor.
   */
  public static final StringListSetting cpp_defines=new StringListSetting();
  
  /**
   * The command that invokes the C pre processor.
   */
  public static final StringSetting cpp_command=new StringSetting("clang -C -E");
  
  /**
   * Add the VCT library options to the given option parser.
   * @param clops Option parser.
   */
  public static void add_options(OptionParser clops){
    clops.add(keep_temp_files.getEnable("keep temporary files"),"keep");
    clops.add(detailed_errors.getEnable("produce detailed error messages"),"detail");
    clops.add(backend_file.getAssign("filename for storing the back-end input"),"encoded");
    clops.add(vct.boogie.Main.boogie_module.getAssign("name of the boogie environment module"),"boogie-module");
    clops.add(vct.boogie.Main.dafny_module.getAssign("name of the dafny environment module"),"dafny-module");
    clops.add(vct.boogie.Main.boogie_timeout.getAssign("boogie time limit"),"boogie-limit");
    clops.add(vct.boogie.Main.chalice_module.getAssign("name of the chalice environment module"),"chalice-module");
    clops.add(vct.silver.SilverBackend.silver_module.getAssign("name of the silver environment module"),"silver-module");
    clops.add(vct.silver.SilverBackend.silicon_z3_timeout.getAssign("Set the Z3 timeout for Silicon"),"silicon-z3-timeout");
//    clops.add(pvl_type_check.getDisable("disable type check in PVL parser"),"no-pvl-check");
    clops.add(assume_single_group.getEnable("enable single group assumptions"),"single-group");
    clops.add(auto_barrier.getDisable("Disable automatic permission revokation for barriers"),"disable-auto-barrier");
    clops.add(enable_resource_check.getDisable("disable barrier resource check during kernel verification"),"disable-resource-check");
    clops.add(enable_post_check.getDisable("disable barrier post check during kernel verification"),"disable-post-check");
    clops.add(witness_constructors.getEnable("use constructors for witnesses"),"witness-constructors");
    clops.add(witness_constructors.getDisable("inline constructors for witnesses"),"witness-inline");
    clops.add(Configuration.modulepath.getAppendOption("configure path for finding back end modules"),"module-path");
    clops.add(cpp_command.getAssign("set the C Pre Processor command"),"cpp");
    clops.add(cpp_include_path.getAppendOption("add to the CPP include path"),'I',"include");
    clops.add(cpp_defines.getAppendOption("add to the CPP defined variables"),'D');
  }

  /**
   * Contains the absolute path to the home of the tool set installation.
   */
  private static Path home;
 
  private static Path tool_home;
  
  private static boolean windows;
 
  static {
    String tmp=System.getenv("VCT_HOME");
    if (tmp==null){
      ClassLoader loader=Configuration.class.getClassLoader();
      URL url=loader.getResource("vct/util/Configuration.class");
      File f=null;
      Debug("origin is %s", f);
      switch(url.getProtocol()){
      case "file":
        f=new File(url.getFile());
        for(int i=0;i<5;i++) f=f.getParentFile();
      case "jar":
        tmp=url.getFile();
        if (tmp.startsWith("file:")) tmp=tmp.substring(5); else break;
        int idx=tmp.indexOf("!");
        if (idx<0) break;
        tmp=tmp.substring(0,idx);
        f=new File(tmp);
        f=f.getParentFile();
      }
      if (f==null){
        throw new Error("Could not deduce VCT_HOME");
      }
      Debug("home is %s", f);
      tmp=f.toString();
      // Remove the file: prefix that shows up while executing under ant.
      if (tmp.startsWith("file:")) tmp=tmp.substring(5);
    }
    home=Paths.get(tmp).toAbsolutePath().normalize();
    if (!home.toFile().isDirectory()){
      throw new Error("VCT_HOME value "+tmp+" is not a directory");
    }
    Path module_deps=home.resolve("deps").resolve("modules");
    if (!module_deps.toFile().isDirectory()){
      module_deps=home.getParent().getParent().resolve(Paths.get("modules"));
      if (!module_deps.toFile().isDirectory()){
        hre.System.Fail("dependency modules not found");
      }
    }
    tool_home=module_deps.getParent();
    modulepath=new StringListSetting(module_deps.toString());
  }

  /**
   * Get the home of the VerCors Tool installation.
   * @return VerCors Tool home.
   */
  public static Path getHome(){
    return home;
  }
  
  /**
   * Get the home of the Third party tools installation.
   * @return Tools home.
   */
  public static Path getToolHome(){
    return tool_home;
  }

  /**
   * 
   */
  public static ModuleShell getShell(String ... modules){
    ModuleShell shell;
    try {
      if (modules.length==0){
        return new ModuleShell();
      }
      //System.err.printf("home: %s%ngeneric:%s%nsystem:%s%n",getHome(),generic_deps,system_deps);
      shell = new ModuleShell(getHome().resolve(Paths.get("modules")));
      for (String p:modulepath){
        shell.send("module use %s",p);
      }
      for (String m:modules){
        shell.send("module load %s",m);
      }
      shell.send("module list -t");
      shell.send("echo ==== 1>&2");
      String line;
      for(;;){
        Message msg=shell.recv();
        if (msg.getFormat().equals("stdout: %s")){
          line=(String)msg.getArg(0);
          continue;
        }
        if (msg.getFormat().equals("stderr: %s")){
          line=(String)msg.getArg(0);
          if (line.equals("Currently Loaded Modulefiles:")) break;
          if (line.equals("No Modulefiles Currently Loaded.")) break;
          continue;
        }
        Warning("unexpected shell response");
        Abort(msg.getFormat(),msg.getArgs());
      }
      for(;;){
        Message msg=shell.recv();
        Debug(msg.getFormat(),msg.getArgs());
        String fmt=msg.getFormat();
        if (fmt.equals("stderr: %s")||fmt.equals("stdout: %s")){
          line=(String)msg.getArg(0);
          if (line.contains("echo ====")) continue;
          if (line.contains("====")) break;
          Progress("module %s loaded",line);
          continue;
        } 
        Warning("unexpected shell response");
        Abort(msg.getFormat(),msg.getArgs());
      }
    } catch (IOException e) {
      e.printStackTrace();
      throw new Error(e.getMessage());
    }
    return shell;
  }
  /**
   * Get the syntax that is to be used for diagnostic output.
   */
  public static Syntax getDiagSyntax(){
    return JavaSyntax.getJava(JavaDialect.JavaVerCors);
  }
}
