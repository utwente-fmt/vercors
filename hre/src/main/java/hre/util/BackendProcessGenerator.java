package hre.util;

import hre.config.Configuration;
import hre.io.MessageProcessEnvironment;

import java.io.File;
import java.io.IOException;
import java.util.List;
import java.util.stream.Collectors;

import static hre.lang.System.Failure;

public class BackendProcessGenerator {
    /**
     * Create a process for the Jacoco CLI client included with VerCors. The CLI client can produce html reports and xml
     * files from .exec traces, as produced by the jacoco agent.
     */
    public static MessageProcessEnvironment getJacocoCli() throws IOException {
        List<String> pathsCandidates = FileHelper.getClassPathElements().stream()
                .filter(cp -> cp.contains("org.jacoco.cli"))
                .collect(Collectors.toList());

        if (pathsCandidates.size() > 1) {
            throw Failure("Multiple candidates found for Jacoco CLI in classpath");
        } else if (pathsCandidates.isEmpty()) {
            throw Failure("Jacoco CLI not found in classpath");
        }

        MessageProcessEnvironment env = new MessageProcessEnvironment(FileHelper.getThisJava().getAbsolutePath());
        File jacocoCliPath = new File(pathsCandidates.get(0));
        env.addArg("-jar", jacocoCliPath.getAbsolutePath());
        return env;
    }

    public static MessageProcessEnvironment getZ3() throws IOException {
        MessageProcessEnvironment env = new MessageProcessEnvironment("z3");
        env.setTemporaryWorkingDirectory();
        env.addPath(FileHelper.getZ3Path().getAbsolutePath());
        return env;
    }

    public static MessageProcessEnvironment getThisVerCors(List<String> javaArgs) throws IOException {
        MessageProcessEnvironment env = new MessageProcessEnvironment(FileHelper.getThisJava().getAbsolutePath());
        env.setTemporaryWorkingDirectory();
        // We need the current path, as vercors e.g. needs clang on the path.
        String[] thisPath = System.getenv("PATH").split(File.pathSeparator);
        for(String thisPathPart : thisPath) {
            env.addPath(thisPathPart);
        }
        env.addArg("-Xss128M");
        env.addArg("-cp", FileHelper.getClassPath());

        if (javaArgs != null) {
            for (String javaArg : javaArgs) {
                env.addArg(javaArg);
            }
        }

        env.addArg("vct.main.Main");
        if(System.getenv("TEMP") != null) {
            env.setEnvironmentVar("TEMP", System.getenv("TEMP"));
        }
        return env;
    }

    public static MessageProcessEnvironment getCarbon() throws IOException {
        MessageProcessEnvironment env = new MessageProcessEnvironment(FileHelper.getThisJava().getAbsolutePath());
        env.setTemporaryWorkingDirectory();
        env.addArg("-Xss128M");
        env.addArg("-cp", FileHelper.getClassPath());
        env.addArg("viper.api.CarbonVerifier");
        return env;
    }

    public static MessageProcessEnvironment getSilicon() throws IOException {
        MessageProcessEnvironment env = new MessageProcessEnvironment(FileHelper.getThisJava().getAbsolutePath());
        env.setTemporaryWorkingDirectory();
        env.addArg("-Xss128M");
        env.addArg("-cp", FileHelper.getClassPath());
        env.addArg("viper.api.SiliconVerifier");
        return env;
    }

}
