package integration.helper;

import hre.lang.Failure;
import hre.lang.ISystem;
import hre.lang.LogLevel;
import hre.lang.SystemNonStatic;

import java.io.PrintWriter;
import java.io.StringWriter;

class SystemListener implements ISystem {
  private ISystem system = new SystemNonStatic();
  private final String expectedVerdictMessage;
  private Boolean foundExpectedMessage = false;

  public SystemListener(String expectedVerdictMessage){
    this.expectedVerdictMessage = expectedVerdictMessage;
  }

  @Override
  public void Verdict(String format, Object... args) {
    if(format.equals(expectedVerdictMessage)){
      foundExpectedMessage=true;
    }
    var message = String.format(format, args);
    if(message.equals(expectedVerdictMessage)){
      foundExpectedMessage=true;
    }
    system.Verdict(format,args);
  }

  public String getExpectedVerdictMessage() {
    return expectedVerdictMessage;
  }

  public Boolean getFoundExpectedMessage() {
    return foundExpectedMessage;
  }

  @Override
  public void DebugException(Throwable e) {
    StringWriter sw = new StringWriter();
    e.printStackTrace(new PrintWriter(sw));
    for (String line : sw.toString().split("\\r?\\n")) {
      Warning("%s", line);
    }
  }

  @Override
  public void setOutputStream(Appendable a, LogLevel level) {
    system.setOutputStream(a,level);
  }

  @Override
  public void setErrorStream(Appendable a, LogLevel level) {
    system.setErrorStream(a,level);
  }

  @Override
  public PrintWriter getLogLevelOutputWriter(LogLevel level) {
    return system.getLogLevelOutputWriter(level);
  }

  @Override
  public PrintWriter getLogLevelErrorWriter(LogLevel level) {
    return system.getLogLevelErrorWriter(level);
  }

  @Override
  public void addDebugFilterByClassName(String className) {
    system.addDebugFilterByClassName(className);
  }

  @Override
  public void addDebugFilterByLine(String classLineCombo) {
    system.addDebugFilterByLine(classLineCombo);
  }

  @Override
  public void Abort(String format, Object... args) {
    system.Abort(format,args);
  }

  @Override
  public void Fail(String format, Object... args) {
    system.Fail(format,args);
  }

  @Override
  public void Debug(String format, Object... args) {
    system.Debug(format,args);
  }

  @Override
  public void Progress(String format, Object... args) {
    system.Progress(format,args);
  }

  @Override
  public void Output(String format, Object... args) {
    system.Output(format,args);
  }

  @Override
  public void Warning(String format, Object... args) {
    system.Warning(format,args);
  }

  @Override
  public Failure Failure(String format, Object... args) {
    return system.Failure(format,args);
  }

}
