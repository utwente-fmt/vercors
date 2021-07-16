package hre.io;

import java.io.IOException;
import java.io.OutputStream;
import java.util.Arrays;

public class SplittingOutputStream extends OutputStream {

    private final OutputStream out[];

    public SplittingOutputStream(OutputStream... out) {
        this.out = Arrays.copyOf(out, out.length);
    }

    @Override
    public void write(int b) throws IOException {
        for (OutputStream s : out) {
            s.write(b);
        }
    }

}
