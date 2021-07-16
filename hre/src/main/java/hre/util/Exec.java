package hre.util;

import hre.lang.HREError;

import java.io.File;
import java.io.IOException;

import static hre.lang.System.Debug;

/**
 * Provides methods that can execute an external program.
 *
 * @author Stefan Blom
 */
public class Exec {

    /**
     * Execute an external program with IO from and to files.
     *
     * @param stdin        Optional input file.
     * @param stdout       File for storing standard output.
     * @param stderr       File for storing standard error.
     * @param command_line An array of strings comprising the command line.
     * @return The exit code of the process, or -1 if interrupted.
     */
    public static int exec(File stdin, File stdout, File stderr, String... command_line) {
        Runtime runtime = Runtime.getRuntime();
        String OS = java.lang.System.getProperty("os.name");
        if (OS.startsWith("Windows")) {
            File tmp = new File(command_line[0]);
            if (!tmp.exists()) {
                command_line[0] = command_line[0] + ".bat";
            }
        }
        Process process;
        try {
            process = runtime.exec(command_line);
        } catch (IOException e) {
            Debug("exec yields %s", e);
            return -1;
        }
        CopyThread stdin_copy, stdout_copy, stderr_copy;
        try {
            if (stdin != null) stdin_copy = new CopyThread(stdin, process.getOutputStream());
            else stdin_copy = null;
            stdout_copy = new CopyThread(process.getInputStream(), stdout);
            stdout_copy.start();
            stderr_copy = new CopyThread(process.getErrorStream(), stderr);
            stderr_copy.start();
        } catch (IOException e) {
            Debug("IO setup failed %s", e);
            return -1;
        }
        try {
            // wait for process and output.
            int exitcode = process.waitFor();
            if (stdin_copy != null) stdin_copy.join();
            stdout_copy.join();
            if (stdout_copy.getError() != null) {
                throw new HREError("stdout error: %s", stdout_copy.getError());
            }
            stderr_copy.join();
            if (stderr_copy.getError() != null) {
                throw new HREError("stderr error: %s", stderr_copy.getError());
            }
            return exitcode;
        } catch (InterruptedException e) {
            hre.lang.System.Warning("interrupted!");
            process.destroy();
            return -1;
        }
    }

}
