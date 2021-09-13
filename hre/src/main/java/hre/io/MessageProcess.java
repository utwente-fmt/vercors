package hre.io;

import java.io.IOException;
import java.io.PrintStream;
import java.nio.file.Path;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import static hre.lang.System.Debug;

/**
 * Provides communication with a interactive external process.
 *
 * @author sccblom, phbos
 */
public class MessageProcess {
    private PrintStream processStdin;
    private Process process;
    private BlockingQueue<Message> processOutputLineQueue;
    private ProcessWatcher processWatcher;

    /**
     * Wraps a system process as an interactive resources.
     * Every input message is printed to the input of the process.
     * Every line on the standard error and standard output is returned
     * as a reply message.
     *
     * @param argv
     */
    public MessageProcess(Path workingDirectory, String[] argv, String[] env) {
        processOutputLineQueue = new LinkedBlockingQueue<Message>();

        Runtime runtime = Runtime.getRuntime();
        try {
            process = runtime.exec(argv, env, workingDirectory == null ? null : workingDirectory.toFile());
        } catch (IOException e) {
            processOutputLineQueue.add(new Message("exec error %s", e.getMessage()));
            return;
        }
        Thread stdout_parser = new StreamConverterThread("stdout", process.getInputStream(), processOutputLineQueue);
        stdout_parser.start();
        Thread stderr_parser = new StreamConverterThread("stderr", process.getErrorStream(), processOutputLineQueue);
        stderr_parser.start();

        processStdin = new PrintStream(process.getOutputStream());
        processWatcher = new ProcessWatcher(process, processOutputLineQueue, stdout_parser, stderr_parser);
        processWatcher.start();

        new Thread(() -> {
            try {
                /* This time is a little bit under the max no-output time of our CI */
                if(!process.waitFor(8, TimeUnit.MINUTES)) {
                    processOutputLineQueue.add(new Message("killed"));
                    process.destroyForcibly();
                }
            } catch (InterruptedException e) {
                // Re-set the flag; thread ends here.
                Thread.currentThread().interrupt();
            }
        }).start();
    }

    public void send(String format, Object... args) {
        String message = String.format(format, args);
        Debug("sending \"%s\"", message);
        processStdin.printf("%s%n", message);
        processStdin.flush();
    }

    public Message recv() {
        Message result = null;
        while (result == null) try {
            result = processOutputLineQueue.take();
        } catch (InterruptedException e) {
        }
        return result;
    }
}
