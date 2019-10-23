package vct.logging;

import hre.ast.Origin;

public class ExceptionMessage extends AbstractMessage {
  
  public final Origin origin;
  
  private Exception exception;

  public ExceptionMessage(Exception exception,Origin origin) {
    this.exception=exception;
    this.origin=origin;
    fatal=true;
  }
  public ExceptionMessage(Exception exception) {
    this(exception,null);
  }

  public Exception getException(){
    return exception;
  }
  
  @Override
  public void accept(MessageVisitor visitor){
    visitor.visit(this);
  }

}
