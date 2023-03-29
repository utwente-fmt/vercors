package hre.unix;

import com.sun.jna.Structure;

import java.util.List;

@Structure.FieldOrder({"sec", "usec"})
public class Timeval extends Structure {
    public long sec = 0;
    public long usec = 0;

    public long toUsec() {
        return sec * 1000000L + usec;
    }

    @Override
    public String toString() {
        return String.format("%d.%06ds", sec, usec);
    }
}
