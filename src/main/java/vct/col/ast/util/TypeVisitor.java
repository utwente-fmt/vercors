package vct.col.ast.util;

import vct.col.ast.type.ClassType;
import vct.col.ast.type.FunctionType;
import vct.col.ast.type.PrimitiveType;
import vct.col.ast.type.RecordType;

public interface TypeVisitor {

  public void visit(PrimitiveType t);
  
  public void visit(ClassType t);

  public void visit(RecordType t);
  
  public void visit(FunctionType t);

}
