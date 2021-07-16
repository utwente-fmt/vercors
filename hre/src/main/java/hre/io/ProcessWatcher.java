package hre.io;

import java.util.Queue;

import static hre.lang.System.Debug;

/**
 * This class implements a thread that waits for the completion
 * of another process and zero or more threads.
 *
 * @author Stefan Blom
 */
public class ProcessWatcher extends Thread {

    private Process process;
    private Queue<Message> queue;
    private Thread threads[];
    private boolean finished;

    /**
     * Create a thread that will wait for completion of a process and threads.
     * <p>
     * This run method in this class will wait for the process to terminate.
     * If the wait is interrupted then the process is killed. After having
     * obtained the exit code the process will join with the given threads
     * and finally report the exit code over the queue with format "exit %d".
     * <p>
     * The intended use of waiting for threads is to allow threads that process
     * the process' output to complete their work before the exit code is
     * reported.
     *
     * @param process The process to wait for.
     * @param queue   The queue where the exit status of the process will be reported.
     * @param threads The threads to wait for before reporting the exit code.
     */
    public ProcessWatcher(Process process, Queue<Message> queue, Thread... threads) {
        this.process = process;
        this.queue = queue;
        this.threads = threads;
        this.finished = false;
    }

    public void run() {
        int exitcode;
        for (; ; )
            try {
                // wait for process and output.
                exitcode = process.waitFor();
                Debug("got exit");
                break;
            } catch (InterruptedException e) {
                // if the process is interrupted, we kill it and wait.
                process.destroy();
            }
        for (int i = 0; i < threads.length; i++) {
            // once the process had died, we wait for the otehr threads,
            // most likely the threads that process output.
            for (; ; )
                try {
                    Debug("wait for %d", i);
                    threads[i].join();
                    Debug("completed");
                    break;
                } catch (InterruptedException e) {
                }
        }
        // thus we try to make sure that the exit message is the last message.
        Debug("queueing exit");
        queue.add(new Message("exit %d", exitcode));
        Debug("process cleanup complete");
        finished = true;
    }

    public boolean getFinished() {
        return finished;
    }
}
