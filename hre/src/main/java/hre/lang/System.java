package hre.lang;

import java.io.PrintWriter;

/**
 * This class provides a way of providing feedback.
 */
public class System {

    private static ISystem currentSystem;

    // This method is for dependence injection. It should only be used in Main and in testing
    public static void setCurrentSystem(ISystem currentSystem) {
        System.currentSystem = currentSystem;
    }

    public static void setOutputStream(Appendable a, LogLevel level) {
        currentSystem.setOutputStream(a,level);
    }

    public static void setErrorStream(Appendable a, LogLevel level) {
        currentSystem.setErrorStream(a,level);
    }

    public static PrintWriter getLogLevelOutputWriter(LogLevel level) {
        return currentSystem.getLogLevelOutputWriter(level);
    }

    public static PrintWriter getLogLevelErrorWriter(LogLevel level) {
        return currentSystem.getLogLevelOutputWriter(level);
    }

    /**
     * Show debug messages from a particular class
     *
     * @param className The name of the class to filter for
     */
    public static void addDebugFilterByClassName(String className) {
        currentSystem.addDebugFilterByClassName(className);
    }

    /**
     * Show debug messages from a particular line in a particular class
     *
     * @param classLineCombo The class name and line number, in the format Class:no
     */
    public static void addDebugFilterByLine(String classLineCombo) {
        currentSystem.addDebugFilterByLine(classLineCombo);
    }


    /**
     * Emit an error message, print stack trace and abort.
     * <p>
     * This method is meant for internal errors which are fatal
     * and may be reported as bugs.
     *
     * @param format The formatting of the message.
     * @param args   The arguments to be formatted.
     */
    public static void Abort(String format, Object... args) {
        currentSystem.Abort(format,args);
    }

    /**
     * Emit an error message and abort.
     * <p>
     * This function is meant to be used for external error conditions,
     * such as bad input.
     */
    public static void Fail(String format, Object... args) {
        currentSystem.Fail(format,args);
    }

    /**
     * Emit a verdict message, used only for the final verdict.
     */
    public static void Verdict(String format, Object... args) {
        currentSystem.Verdict(format,args);
    }


    /**
     * Emit a debug message if the class calling this method is tagged for debugging.
     */
    public static void Debug(String format, Object... args) {
        currentSystem.Debug(format,args);
    }

    /**
     * Emit a stack trace as a debug message
     */
    public static void DebugException(Throwable e) {
        currentSystem.DebugException(e);
    }

    /**
     * Emit a progress message.
     */
    public static void Progress(String format, Object... args) {
        currentSystem.Progress(format,args);
    }

    /**
     * Emit an output message.
     */
    public static void Output(String format, Object... args) {
        currentSystem.Output(format,args);
    }

    /**
     * Emit a warning message.
     */
    public static void Warning(String format, Object... args) {
        currentSystem.Warning(format,args);
    }

    public static Failure Failure(String format, Object... args) {
        return currentSystem.Failure(format,args);
    }
}
