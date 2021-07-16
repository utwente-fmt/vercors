package hre.tools;

import java.lang.management.ManagementFactory;
import java.lang.management.ThreadMXBean;

/**
 * Utility class that calculates time spent between blocks of code. The measured time is CPU time in user mode.
 */
public class TimeKeeper {
    private ThreadMXBean threadBean;
    private long threadId;
    private long lastTime;

    public TimeKeeper(Thread thread) {
        threadBean = ManagementFactory.getThreadMXBean();
        threadId = thread.getId();
        lastTime = threadBean.getThreadUserTime(threadId);
    }

    public TimeKeeper() {
        this(Thread.currentThread());
    }

    public long show() {
        return showNs() / 1_000_000;
    }

    public long showNs() {
        long newTime = threadBean.getThreadUserTime(threadId);
        long difference = newTime - lastTime;
        lastTime = newTime;
        return difference;
    }
}