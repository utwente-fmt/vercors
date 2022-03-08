package hre.lang;

import java.io.PrintWriter;

public interface ISystem {

    void setOutputStream(Appendable a, LogLevel level);

    void setErrorStream(Appendable a, LogLevel level);

    PrintWriter getLogLevelOutputWriter(LogLevel level);

    PrintWriter getLogLevelErrorWriter(LogLevel level);

    void addDebugFilterByClassName(String className);

    void addDebugFilterByLine(String classLineCombo);

    void Abort(String format, Object... args);

    void Fail(String format, Object... args);

    void Verdict(String format, Object... args);

    void Debug(String format, Object... args);

    void DebugException(Throwable e);

    void Progress(String format, Object... args);

    void Output(String format, Object... args);

    void Warning(String format, Object... args) ;

    Failure Failure(String format, Object... args);
}
