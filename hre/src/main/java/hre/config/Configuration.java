package hre.config;

import hre.io.MessageProcessEnvironment;
import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;

import static hre.lang.System.Debug;
import static hre.lang.System.Failure;

/**
 * This class contains the configuration options of the VerCors library.
 *
 * @author Stefan Blom
 *
 */
public class Configuration {
    // When we move to scala 3 this can maybe be refactored to a scala enum
    public enum OS {
        WINDOWS,
        MAC,
        UNIX,
        UNKNOWN
    }

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
     * The option for session type generation
     */
    public static final StringSetting session_file=new StringSetting(null);

    /**
     * Add the VCT library options to the given option parser.
     * @param clops Option parser.
     */
    public static void add_options(OptionParser clops){
        clops.add(keep_temp_files.getEnable("keep temporary files"),"keep");
        clops.add(detailed_errors.getEnable("produce detailed error messages"),"detail");
        clops.add(backend_file.getAssign("filename for storing the back-end input"),"encoded");
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
        clops.add(session_file.getAssign("generate threads from session type"),"session");
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
        File base = getFileOrAbort("/deps/z3/4.8.6");

        switch (getOS()) {
            case WINDOWS:
                return join(base, "Windows NT", "intel", "bin", "z3.exe");
            case MAC:
                return join(base, "Darwin", "x86_64", "bin", "z3");
            default:
                return join(base, "Linux", "x86_64", "bin", "z3");
        }
    }

    public static File getBoogieZ3Path() {
        return getZ3Path();
    }

    public static File getDafnyZ3Path() {
        return getZ3Path();
    }

    public static File getChaliceZ3Path() {
        return getZ3Path();
    }

    public static File getBoogiePath() {
        File base = getFileOrAbort("/deps/boogie/2012-10-22/");

        if(getOS() == OS.WINDOWS) {
            return join(base, "windows", "bin");
        } else {
            return join(base, "unix", "bin");
        }
    }

    public static File getChalicePath() {
        File base = getFileOrAbort("/deps/chalice/2013-12-17/");

        if(getOS() == OS.WINDOWS) {
            return join(base, "windows", "bin");
        } else {
            return join(base, "unix", "bin");
        }
    }

    public static File getDafnyPath() {
        File base = getFileOrAbort("/deps/dafny/1.9.6/");

        if (getOS() == OS.WINDOWS) {
            return join(base, "windows");
        } else {
            return join(base, "unix");
        }
    }

    public static MessageProcessEnvironment getZ3() throws IOException {
        MessageProcessEnvironment env = new MessageProcessEnvironment("z3");
        env.setTemporaryWorkingDirectory();
        env.addPath(getZ3Path().getAbsolutePath());
        return env;
    }

    public static MessageProcessEnvironment getBoogie() throws IOException {
        MessageProcessEnvironment env = new MessageProcessEnvironment("boogie");
        env.setTemporaryWorkingDirectory();
        env.setEnvironmentVar("BOOGIE_Z3_EXE", getBoogieZ3Path().getAbsolutePath());
        env.addPath(getBoogiePath().getAbsolutePath());
        env.addPath(getBoogieZ3Path().getAbsolutePath());
        return env;
    }

    public static MessageProcessEnvironment getDafny() throws IOException {
        MessageProcessEnvironment env = new MessageProcessEnvironment("dafny");
        env.setTemporaryWorkingDirectory();
        env.addPath(getDafnyPath().getAbsolutePath());
        env.addPath(getDafnyZ3Path().getParentFile().getAbsolutePath());
        return env;
    }

    public static MessageProcessEnvironment getChalice() throws IOException {
        MessageProcessEnvironment env = new MessageProcessEnvironment("chalice");
        env.setTemporaryWorkingDirectory();
        env.addPath(getChalicePath().getAbsolutePath());
        env.addPath(getBoogiePath().getAbsolutePath());
        env.addPath(getChaliceZ3Path().getAbsolutePath());
        return env;
    }

    private static File getThisJava() throws IOException {
        File javaHome = new File(System.getProperty("java.home"));
        File javaBin = join(javaHome, "bin");
        File javaExe;

        if((javaExe = join(javaBin, "java")).exists()) {
            return javaExe;
        } else if((javaExe = join(javaBin, "java.exe")).exists()) {
            return javaExe;
        } else {
            throw new IOException("Could not find the current java.");
        }
    }

    public static MessageProcessEnvironment getThisVerCors() throws IOException {
        MessageProcessEnvironment env = new MessageProcessEnvironment(getThisJava().getAbsolutePath());
        env.setTemporaryWorkingDirectory();
        // We need the current path, as vercors e.g. needs clang on the path.
        String[] thisPath = System.getenv("PATH").split(File.pathSeparator);
        for(String thisPathPart : thisPath) {
            env.addPath(thisPathPart);
        }
        env.addArg("-Xss128M");
        env.addArg("-cp", System.getProperty("java.class.path"));
        env.addArg("vct.main.Main");
        return env;
    }

    public static MessageProcessEnvironment getCarbon() throws IOException {
        MessageProcessEnvironment env = new MessageProcessEnvironment(getThisJava().getAbsolutePath());
        env.setTemporaryWorkingDirectory();
        env.addArg("-Xss128M");
        env.addArg("-cp", System.getProperty("java.class.path"));
        env.addArg("viper.api.CarbonVerifier");
        return env;
    }

    public static MessageProcessEnvironment getSilicon() throws IOException {
        MessageProcessEnvironment env = new MessageProcessEnvironment(getThisJava().getAbsolutePath());
        env.setTemporaryWorkingDirectory();
        env.addArg("-Xss128M");
        env.addArg("-cp", System.getProperty("java.class.path"));
        env.addArg("viper.api.SiliconVerifier");
        return env;
    }

    public static OS getOS() {
        String os = System.getProperty("os.name").toLowerCase();
        if (os.contains("win")) {
            return OS.WINDOWS;
        } else if (os.contains("mac")) {
            return OS.MAC;
        } else if (os.contains("nix") || os.contains("linux")) {
            return OS.UNIX;
        } else {
            return OS.UNKNOWN;
        }
    }
}
