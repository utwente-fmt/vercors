package hre.unix;

import com.sun.jna.Structure;

@Structure.FieldOrder({"ru_utime", "ru_stime", "ru_maxrss", "ru_ixrss", "ru_idrss", "ru_isrss", "ru_minflt", "ru_majflt", "ru_nswap", "ru_inblock", "ru_oublock", "ru_msgsnd", "ru_msgrcv", "ru_nsignals", "ru_nvcsw", "ru_nivcsw"})
public class RUsage extends Structure {
    public Timeval ru_utime = new Timeval();
    public Timeval ru_stime = new Timeval();

    public long ru_maxrss = 0;
    public long ru_ixrss = 0;
    public long ru_idrss = 0;
    public long ru_isrss = 0;
    public long ru_minflt = 0;
    public long ru_majflt = 0;
    public long ru_nswap = 0;
    public long ru_inblock = 0;
    public long ru_oublock = 0;
    public long ru_msgsnd = 0;
    public long ru_msgrcv = 0;
    public long ru_nsignals = 0;
    public long ru_nvcsw = 0;
    public long ru_nivcsw = 0;
}