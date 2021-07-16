package col.lang;

import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.concurrent.locks.Condition;
import java.util.concurrent.locks.Lock;
import java.util.concurrent.locks.ReentrantLock;

public class Object implements Runnable {

    private Lock lock;
    private Condition cond;
    private ThreadLocal<Boolean> locked;
    private volatile Thread t;

    public Object() {
        lock = new ReentrantLock();
        cond = lock.newCondition();
        locked = new ThreadLocal<Boolean>();
    }

    public void lock() {
        lock.lock();
        if (locked.get() == null) {
            locked.set(false);
        }
        if (locked.get()) {
            throw new Error("illegal attempt to relock non-reentrant lock");
        }
        locked.set(true);
    }

    public void lock_wait() {
        try {
            cond.await();
        } catch (InterruptedException e) {
            throw new Error("unexpected interrupt");
        }
    }

    public void lock_notify() {
        cond.signalAll();
    }

    public void unlock() {
        lock.lock();
        if (!locked.get()) {
            throw new Error("illegal attempt to unlock while not holding the lock");
        }
        locked.set(false);
        lock.unlock();
        lock.unlock();
    }

    public synchronized boolean isIdle() {
        Method m = null;
        try {
            m = getClass().getMethod("run");
        } catch (NoSuchMethodException e) {
            // OK if it does not exists
        } catch (SecurityException e) {
            return t == null;
        }
        if (m == null || !Modifier.isStatic(m.getModifiers())) {
            throw new Error("checking idle on non-runnable object");
        }
        return t == null;
    }

    public synchronized boolean isRunning() {
        return t != null;
    }

    public synchronized void fork() {
        if (t == null) {
            t = new Thread(this);
            t.start();
        } else {
            throw new Error("task has already been forked");
        }
    }

    public synchronized void join() {
        if (t == null) {
            throw new Error("tasks must be forked before they can be joined");
        } else {
            try {
                t.join();
            } catch (InterruptedException e) {
                throw new Error("unexpected interruption of join");
            }
            t = null;
        }
    }

    public void run() {
        throw new Error("illegal attempt to fork a thread for a non-runnable object");
    }

}
