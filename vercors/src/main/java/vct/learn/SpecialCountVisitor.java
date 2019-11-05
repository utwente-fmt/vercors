package vct.learn;

import vct.col.ast.stmt.decl.ProgramUnit;

public class SpecialCountVisitor extends CountVisitor {
  
  protected NodeCounter specialCounter;
  
  public SpecialCountVisitor(ProgramUnit pu) {
    super(pu);
    this.specialCounter = new NodeCounter();
  }
  
  public NodeCounter getSpecialCounter() {
    return specialCounter;
  }
  
}
