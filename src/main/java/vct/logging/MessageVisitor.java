package vct.logging;

public interface MessageVisitor {

  void visit(ExceptionMessage e);

  void visit(TaskBegin begin);

  void visit(TaskEnd end);

  void visit(TaskPhase phase);

  void visit(VerificationResult result);

  void visit(VerCorsError error);
  
}
