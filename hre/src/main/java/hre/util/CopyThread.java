package hre.util;

import java.io.*;

/**
 * Simple asynchronous copy between two streams or one stream and a file.
 *
 * @author sccblom
 */
public class CopyThread extends Thread {

    private InputStream input;
    private boolean close_in = false;
    private OutputStream output;
    private boolean close_out = false;
    private IOException err = null;

    /**
     * Create a copying thread from a file to a stream.
     *
     * @param input  The input File.
     * @param output The output stream.
     * @throws FileNotFoundException If the file cannot be opened for reading.
     */
    public CopyThread(File input, OutputStream output) throws FileNotFoundException {
        this.input = new FileInputStream(input);
        close_in = true;
        this.output = output;
        setDaemon(true);
    }

    /**
     * Create a copying thread from a stream to a file.
     *
     * @param input  The input stream.
     * @param output The output File.
     * @throws FileNotFoundException If the output file cannot be opened for writing.
     */
    public CopyThread(InputStream input, File output) throws FileNotFoundException {
        this.input = input;
        this.output = new FileOutputStream(output);
        close_out = true;
        setDaemon(true);
    }

    /**
     * Create a copying thread from a stream to a stream.
     *
     * @param input  The input stream.
     * @param output The output Stream.
     */
    public CopyThread(InputStream input, OutputStream output) {
        this.input = input;
        this.output = output;
        setDaemon(true);
    }

    public void run() {
        while (err == null) {
            try {
                int ch = input.read();
                if (ch < 0) break;
                output.write(ch);
            } catch (IOException e) {
                err = e;
            }
        }
        try {
            if (close_in) input.close();
        } catch (IOException e) {
            if (err == null) err = e;
        }
        try {
            if (close_out) output.close();
        } catch (IOException e) {
            if (err == null) err = e;
        }
    }

    /**
     * Retrieve error status.
     *
     * @return Upon successfull competion null is returned. If
     * the thread was terminated due to an exception then
     * that exception will be returned.
     */
    public IOException getError() {
        return err;
    }
}
