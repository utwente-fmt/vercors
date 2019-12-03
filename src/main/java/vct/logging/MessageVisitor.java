package vct.logging;

public interface MessageVisitor {

  public void visit(ExceptionMessage e);

  public void visit(TaskBegin begin);

  public void visit(TaskEnd end);

  public void visit(TaskPhase phase);

  public void visit(VerificationResult result);

  public void visit(VerCorsError error);
  
}
