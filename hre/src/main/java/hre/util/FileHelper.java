package hre.util;

import java.io.File;
import java.io.IOException;
import java.net.URISyntaxException;
import java.net.URL;
import java.util.Arrays;
import java.util.List;
import java.util.stream.Collectors;

import static hre.lang.System.*;

public class FileHelper {

    //To ensure this class can not be instantiated.
    private FileHelper(){}

    private static File getFile(String file) {
        try {
            Debug("getting resource %s", file);
            URL resource = FileHelper.class.getResource(file);
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

    public static File[] getVeyMontFiles()  {
        File bar = getFileOrAbort("/include/barrier.pvl");
        File chan = getFileOrAbort("/include/channel.pvl");
        return new File[] {bar,chan};
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

    public static File getBoogiePath() {
        File base = getFileOrAbort("/deps/boogie/1.0.0.0-carbon");

        switch (getOS()) {
            case WINDOWS:
                return join(base,"Windows", "Boogie.exe");
            case UNIX:
                return join(base, "Linux", "Boogie");
            case MAC:
                return join(base, "Darwin", "Boogie");
            default:
                Abort("Could not find boogie for unknown architecture");
                return null;
        }
    }

    /**
     * Returns the java class path, with its elements separated by colons
     */
    public static String getClassPath() {
        return System.getProperty("java.class.path");
    }

    /**
     * Returns all elements of the classpath of the currently running program.
     */
    public static List<String> getClassPathElements() {
        return Arrays.asList(getClassPath()
                .split(String.valueOf(File.pathSeparatorChar)));
    }

    /**
     * Computes the path of the jacoco agent included with VerCors. The agent is used for instrumenting an actual java process.
     */
    public static File getJacocoAgentPath() {
        List<String> pathsCandidates = getClassPathElements().stream()
                .filter(cp -> cp.contains("org.jacoco.agent"))
                .collect(Collectors.toList());

        if (pathsCandidates.size() > 1) {
            throw Failure("Multiple candidates found for jacoco agent in classpath");
        } else if (pathsCandidates.isEmpty()) {
            throw Failure("Jacoco agent not found in classpath");
        }
        return new File(pathsCandidates.get(0));
    }

    public static File getThisJava() throws IOException {
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
