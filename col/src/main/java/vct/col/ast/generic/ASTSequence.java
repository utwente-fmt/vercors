package vct.col.ast.generic;

public interface ASTSequence<T extends ASTSequence<T>> extends Iterable<ASTNode> {

  T add(ASTNode item);
  
  int size();
  
  ASTNode get(int i);
  
}
