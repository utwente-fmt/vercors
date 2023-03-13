package vct.parsers.transform.systemctocol.util;

public class Constants {

    // =================== ALL PRIMITIVE CHANNELS =================== //

    /** Class name of a FIFO queue with integer type in the SystemC system */
    public static final String CLASS_FIFO_INT = "sc_fifo_int";
    /** Class name of a FIFO queue with boolean type in the SystemC system */
    public static final String CLASS_FIFO_BOOL = "sc_fifo_bool";
    /** Class name of a signal channel with integer type in the SystemC system */
    public static final String CLASS_SIGNAL_INT = "sc_signal_int";
    /** Class name of a signal channel with boolean type in the SystemC system */
    public static final String CLASS_SIGNAL_BOOL = "sc_signal_bool";

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
