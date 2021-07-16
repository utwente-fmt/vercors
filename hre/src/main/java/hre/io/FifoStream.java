package hre.io;

import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.util.Arrays;
import java.util.concurrent.LinkedBlockingQueue;

public class FifoStream {

    private Input in;
    private Output out;
    private LinkedBlockingQueue<byte[]> queue;

    public FifoStream(int buf) {
        queue = new LinkedBlockingQueue<byte[]>();
        in = new Input(queue);
        out = new Output(queue, buf);
    }

    public InputStream getInputStream() {
        return in;
    }

    public OutputStream getOutputStream() {
        return out;
    }

    private static class Input extends InputStream {

        private int len;
        private int last;
        private byte buffer[];
        private LinkedBlockingQueue<byte[]> queue;
        private boolean eof = false;

        protected Input(LinkedBlockingQueue<byte[]> queue) {
            this.queue = queue;
        }

        @Override
        public int read() throws IOException {
            if (eof) return -1;
            last++;
            if (last == len) buffer = null;
            if (buffer == null) {
                try {
                    buffer = queue.take();
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                    return -1;
                }
                len = buffer.length;
                if (len == 0) {
                    eof = true;
                    return -1;
                }
                last = 0;
            }
            return buffer[last];
        }

    }

    private static class Output extends OutputStream {

        private int len;
        private int used;
        private byte buffer[];
        private LinkedBlockingQueue<byte[]> queue;

        protected Output(LinkedBlockingQueue<byte[]> queue, int len) {
            this.queue = queue;
            this.len = len;
        }

        @Override
        public void write(int b) throws IOException {
            if (buffer == null) {
                buffer = new byte[len];
                used = 0;
            }
            buffer[used] = (byte) b;
            used++;
            if (used == len) {
                try {
                    queue.put(buffer);
                } catch (InterruptedException e) {
                }
                buffer = null;
            }
        }

        @Override
        public void flush() {
            if (buffer != null && used != 0) {
                try {
                    queue.put(Arrays.copyOf(buffer, used));
                } catch (InterruptedException e) {
                }
                used = 0;
            }
        }

        @Override
        public void close() {
            flush();
            try {
                queue.put(new byte[0]);
            } catch (InterruptedException e) {
            }
            queue = null;
        }
    }
}
