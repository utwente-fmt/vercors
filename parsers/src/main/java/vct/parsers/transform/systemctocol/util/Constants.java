package vct.parsers.transform.systemctocol.util;

public class Constants {

    // =================== ALL PRIMITIVE CHANNELS =================== //

    /** Index of the update method in all primitive channel classes */
    public static final int PRIMITIVE_UPDATE_METHOD_INDEX = 0;

    // ======================== FIFO CHANNEL ======================== //

    // Method indices
    /** Index of the FIFO channel's read method */
    public static final int FIFO_READ_METHOD = 1;
    /** Index of the FIFO channel's write method */
    public static final int FIFO_WRITE_METHOD = 2;

    // Field indices
    /** Index of the FIFO channel's buffer field */
    public static final int FIFO_BUFFER = 0;
    /** Index of the FIFO channel's num_read field */
    public static final int FIFO_NUM_READ = 1;
    /** Index of the FIFO channel's written field */
    public static final int FIFO_WRITTEN = 2;

    // Event indices
    /** Index of the FIFO channel's read event */
    public static final int FIFO_READ_EVENT = 0;
    /** Index of the FIFO channel's write event */
    public static final int FIFO_WRITE_EVENT = 1;

    // ======================= SIGNAL CHANNEL ======================= //

    // Method indices
    /** Index of the signal channel's read method */
    public static final int SIGNAL_READ_METHOD = 1;
    /** Index of the signal channel's write method */
    public static final int SIGNAL_WRITE_METHOD = 2;

    // Event indices
    /** Index of the signal channel's read method */
    public static final int SIGNAL_WRITE_EVENT = 0;
}
