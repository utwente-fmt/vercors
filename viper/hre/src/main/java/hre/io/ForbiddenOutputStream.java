package hre.io;

import java.io.IOException;
import java.io.OutputStream;
import java.io.PrintStream;

public class ForbiddenOutputStream extends OutputStream {
  private final PrintStream oldStream;

  public ForbiddenOutputStream(PrintStream oldStream) {
    this.oldStream = oldStream;
  }

  @Override
  public void write(int i) throws IOException {
    oldStream.println("Please do not write to stdout or stderr explicitly! " +
            "You are welcome to leave debugging information in, but use hre.lang.System.Debug instead. " +
            "If you must use a stream, use hre.lang.System.getLoggingLevelOutputStream instead.");
    StackTraceElement[] elements = Thread.currentThread().getStackTrace();

    for(StackTraceElement element : elements) {
      oldStream.println(String.format("at %s:%d", element.getFileName(), element.getLineNumber()));
    }

    System.exit(1);
  }
}
