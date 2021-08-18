package hre.lang;

import hre.config.Configuration;

import java.io.IOException;
import java.io.PrintWriter;
import java.io.StringWriter;
import java.io.Writer;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Map;

public class SystemNonStatic implements ISystem {

    private Map<Appendable, LogLevel> outputStreams = new HashMap<>();
    private Map<Appendable, LogLevel> errorStreams = new HashMap<>();

    private HashSet<String> debugFilterByClassName = new HashSet<>();
    private HashSet<String> debugfilterByLine = new HashSet<>();

    // Here for performance reasons: kept in sync with whether any attached output/error stream has logging level
    // debug or more verbose.
    private boolean DEBUGGER_ATTACHED = false;

    private HashSet<LogWriter> activeLogWriters = new HashSet<>();

    public void setOutputStream(Appendable a, LogLevel level) {
        if (level.getOrder() >= LogLevel.Debug.getOrder()) {
            DEBUGGER_ATTACHED = true;
        }

        outputStreams.put(a, level);
    }

    public void setErrorStream(Appendable a, LogLevel level) {
        if (level.getOrder() >= LogLevel.Debug.getOrder()) {
            DEBUGGER_ATTACHED = true;
        }

        errorStreams.put(a, level);
    }

    private class LogWriter extends Writer {
        private String buffer = "";
        private LogLevel level;
        private Map<Appendable, LogLevel> outputs;

        LogWriter(LogLevel level, Map<Appendable, LogLevel> outputs) {
            this.level = level;
            this.outputs = outputs;
        }

        @Override
        public void write(char[] chars, int start, int end) throws IOException {
            buffer += new String(chars, start, end);
            writeLines();
        }

        private void writeLines() {
            String[] lines = buffer.split("\\r?\\n", -1);
            buffer = lines[lines.length - 1];

            for (int i = 0; i < lines.length - 1; i++) {
                log(level, outputs, "%s", lines[i]);
            }
        }

        @Override
        public void flush() throws IOException {
            // Refuse, as flushing in this context may cause an additional unwanted newline.
        }

        @Override
        public void close() throws IOException {
            doFlush();
            activeLogWriters.remove(this);
        }

        private void doFlush() {
            if (!buffer.equals("")) {
                String toLog = buffer;
                buffer = "";
                log(level, outputs, "%s", toLog);
            }
        }
    }

    private PrintWriter getLogLevelWriter(LogLevel level, Map<Appendable, LogLevel> outputs) {
        LogWriter writer = new LogWriter(level, outputs);
        activeLogWriters.add(writer);
        return new PrintWriter(writer);
    }

    public PrintWriter getLogLevelOutputWriter(LogLevel level) {
        return getLogLevelWriter(level, outputStreams);
    }

    public PrintWriter getLogLevelErrorWriter(LogLevel level) {
        return getLogLevelWriter(level, errorStreams);
    }

    /**
     * Show debug messages from a particular class
     *
     * @param className The name of the class to filter for
     */
    public void addDebugFilterByClassName(String className) {
        debugFilterByClassName.add(className);
    }

    /**
     * Show debug messages from a particular line in a particular class
     *
     * @param classLineCombo The class name and line number, in the format Class:no
     */
    public void addDebugFilterByLine(String classLineCombo) {
        debugfilterByLine.add(classLineCombo);
    }

    private boolean needLineClear = false;

    private void log(LogLevel level, Map<Appendable, LogLevel> outputs, String format, Object... args) {
        // Only format the string (expensive) when the message is actually outputted
        String message = null;

        for (Map.Entry<Appendable, LogLevel> entry : outputs.entrySet()) {
            if (entry.getValue().getOrder() >= level.getOrder()) {
                try {
                    if (message == null) {
                        for (LogWriter writer : activeLogWriters) {
                            writer.doFlush();
                        }
                        message = level.getShorthand() + String.format(format, args);
                    }

                    if(needLineClear) {
                        entry.getKey().append("\033[0K");
                        needLineClear = false;
                    }

                    entry.getKey().append(message);

                    if(level == LogLevel.Progress && Configuration.ansi.get()) {
                        entry.getKey().append("\r");
                        needLineClear = true;
                    } else {
                        entry.getKey().append("\r\n");
                    }
                } catch (IOException e) {
                    if (level != LogLevel.Abort) {
                        Abort("IO Error: %s", e);
                    }
                }
            }
        }
    }

    private StackTraceElement getCallSite() {
        StackTraceElement[] stackTraceElements = Thread.currentThread().getStackTrace();

        int idx = 2;
        while (stackTraceElements[idx].getClassName().equals("hre.lang.System")
                || stackTraceElements[idx].getClassName().equals("vct.col.ast.util.ASTFrame")) {
            idx++;
        }

        return stackTraceElements[idx];
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
    public void Abort(String format, Object... args) {
        log(LogLevel.Abort, errorStreams, format, args);
        throw new HREExitException(1);
    }

    /**
     * Emit an error message and abort.
     * <p>
     * This function is meant to be used for external error conditions,
     * such as bad input.
     */
    public void Fail(String format, Object... args) {
        Verdict(format, args);
        throw new HREExitException(1);
    }

    /**
     * Emit a verdict message, used only for the final verdict.
     */
    public void Verdict(String format, Object... args) {
        StackTraceElement callSite = getCallSite();
        log(LogLevel.Debug, outputStreams, "At %s:%d:", callSite.getFileName(), callSite.getLineNumber());
        log(LogLevel.Result, outputStreams, format, args);
    }


    /**
     * Emit a debug message if the class calling this method is tagged for debugging.
     */
    public void Debug(String format, Object... args) {
        if(DEBUGGER_ATTACHED) {
            // Stack trace computation is expensive, so guard behind boolean.
            StackTraceElement callSite = getCallSite();

            if (debugFilterByClassName.contains(callSite.getClassName())
                    || debugfilterByLine.contains(callSite.getClassName() + ":" + callSite.getLineNumber())
            ) {
                log(LogLevel.Debug, errorStreams, "At %s:%d: ", callSite.getFileName(), callSite.getLineNumber());
                log(LogLevel.Debug, errorStreams, format, args);
            } else {
                log(LogLevel.All, errorStreams, "At %s:%d: ", callSite.getFileName(), callSite.getLineNumber());
                log(LogLevel.All, errorStreams, format, args);
            }
        }
    }

    /**
     * Emit a stack trace as a debug message
     */
    public void DebugException(Throwable e) {
        StringWriter sw = new StringWriter();
        e.printStackTrace(new PrintWriter(sw));
        for (String line : sw.toString().split("\\r?\\n")) {
            Debug("%s", line);
        }
    }

    /**
     * Emit a progress message.
     */
    public void Progress(String format, Object... args) {
        log(LogLevel.Progress, outputStreams, format, args);
    }

    /**
     * Emit an output message.
     */
    public void Output(String format, Object... args) {
        log(LogLevel.Info, outputStreams, format, args);
    }

    /**
     * Emit a warning message.
     */
    public void Warning(String format, Object... args) {
        log(LogLevel.Warning, errorStreams, format, args);
    }

    public Failure Failure(String format, Object... args) {
        StackTraceElement callSite = getCallSite();
        log(LogLevel.Debug, outputStreams, "At %s:%d:", callSite.getFileName(), callSite.getLineNumber());
        log(LogLevel.Result, outputStreams, format, args);
        return new Failure(String.format(format, args));
    }
}
