package vct.util;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.nio.file.Path;
import java.nio.file.Paths;
import java.util.function.Supplier;

import hre.io.MessageProcess;
import vct.antlr4.parser.Parsers;
import vct.col.syntax.JavaDialect;
import vct.col.syntax.JavaSyntax;
import vct.col.syntax.Syntax;
import hre.config.BooleanSetting;
import hre.config.IntegerSetting;
import hre.config.Option;
import hre.config.OptionParser;
import hre.config.StringListSetting;
import hre.config.StringSetting;
import hre.io.Message;

import static hre.lang.System.*;

/**
 * This class contains the configuration options of the VerCors library.
 *
 * @author Stefan Blom
 *
 */
public class Configuration {
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
        clops.add(vct.boogie.Main.boogie_timeout.getAssign("boogie time limit"),"boogie-limit");
        clops.add(vct.silver.SilverBackend.silver_module.getAssign("name of the silver environment module"),"silver-module");
        clops.add(vct.silver.SilverBackend.silicon_z3_timeout.getAssign("Set the Z3 timeout for Silicon"),"silicon-z3-timeout");
        // clops.add(pvl_type_check.getDisable("disable type check in PVL parser"),"no-pvl-check");
        clops.add(assume_single_group.getEnable("enable single group assumptions"),"single-group");
        clops.add(auto_barrier.getDisable("Disable automatic permission revokation for barriers"),"disable-auto-barrier");
        clops.add(enable_resource_check.getDisable("disable barrier resource check during kernel verification"),"disable-resource-check");
        clops.add(enable_post_check.getDisable("disable barrier post check during kernel verification"),"disable-post-check");
        clops.add(witness_constructors.getEnable("use constructors for witnesses"),"witness-constructors");
        clops.add(witness_constructors.getDisable("inline constructors for witnesses"),"witness-inline");
        clops.add(cpp_command.getAssign("set the C Pre Processor command"),"cpp");
        clops.add(cpp_include_path.getAppendOption("add to the CPP include path"),'I',"include");
        clops.add(cpp_defines.getAppendOption("add to the CPP defined variables"),'D');
        clops.add(profiling_option, "profile");
        clops.add(skip.getAppendOption("comma separated list of methods that may be skipped during verification"),"skip");
        clops.add(Parsers.java_version.getAssign("set the version of java used for parsing"),"java");
    }

    public static IntegerSetting profiling=new IntegerSetting(1000);
    public static Option profiling_option=profiling.getOptionalAssign("Enable profiling");

    public static StringListSetting skip=new StringListSetting();

    private static File getFile(String file) {
        try {
            Debug("getting resource %s", file);
            URL resource = Configuration.class.getResource(file);
            return resource == null ? null : new File(resource.toURI());
        } catch (URISyntaxException e) {
            return null;
        }
    }

    private static File getFileOrAbort(String file) {
        File javaFile = getFile(file);

        if(javaFile == null) {
            throw Failure("Could not find required file or directory %s", file);
        } else {
            return javaFile;
        }
    }

    private static File join(File base, String... path) {
        for(String part : path) {
            base = new File(base, part);
        }

        return base;
    }

    public static File getConfigFile(String file) {
        return getFileOrAbort("/config/" + file);
    }

    public static File getCIncludePath() {
        return getFileOrAbort("/include");
    }

    public static File getSelfTestPath(String test) {
        return getFileOrAbort("/selftest/" + test);
    }

    public static File getZ3Path() {
        File base = getFileOrAbort("/deps/z3/4.8.5");
        String os = System.getProperty("os.name");

        if(os.startsWith("Windows")) {
            return join(base, "Windows NT", "intel", "bin", "z3.exe");
        } else if(os.startsWith("Mac")) {
            return join(base, "Darwin", "x86_64", "bin", "z3");
        } else {
            return join(base, "Linux", "x86_64", "bin", "z3");
        }
    }

    public static File getBoogiePath() {
        File base = getFileOrAbort("/deps/boogie/2012-10-22/");
        String os = System.getProperty("os.name");

        if(os.startsWith("Windows")) {
            return join(base, "windows", "bin");
        } else {
            return join(base, "unix", "bin");
        }
    }

    public static File getChalicePath() {
        File base = getFileOrAbort("/deps/chalice/2013-12-17/");
        String os = System.getProperty("os.name");

        if(os.startsWith("Windows")) {
            return join(base, "windows", "bin");
        } else {
            return join(base, "unix", "bin");
        }
    }

    public static MessageProcess getShell(String ... modules){
        return null;
        ModuleShell shell;
        try {
            if (modules.length==0){
                return new ModuleShell();
            }
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
            DebugException(e);
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
