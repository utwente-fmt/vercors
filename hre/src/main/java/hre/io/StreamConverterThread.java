package hre.io;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.Queue;

/**
 * Convert the lines in a stream to a stream of messages.
 *
 * @author sccblom
 */
public class StreamConverterThread extends Thread {

    private BufferedReader in;
    private Queue<Message> queue;
    private String format;

    public StreamConverterThread(String name, InputStream in, Queue<Message> queue) {
        this.in = new BufferedReader(new InputStreamReader(in));
        this.queue = queue;
        format = name + ": %s";
        setDaemon(true);
    }

    public void run() {
        // the convention is that line==null means end-of-file seen.
        String line = format;
        try {
            for (; ; ) {
                line = in.readLine();
                // if end-of-file exit loop;
                if (line == null) break;
                hre.lang.System.Debug("got: " + format, line);
                queue.add(new Message(format, line));
            }
            in.close();
        } catch (IOException e) {
            queue.add(new Message("IOException %s", e.getMessage()));
            // if exception at any place other than during close, try to close.
            if (line != null) try {
                in.close();
            } catch (IOException ee) {
            }
            ;
            return;
        }
    }


}
