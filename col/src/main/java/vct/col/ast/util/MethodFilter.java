package vct.col.ast.util;

import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.Method;
import hre.util.Function;

public class MethodFilter implements Function<ASTNode,Method> {

  public MethodFilter(){}

  @Override
  public Method apply(ASTNode e) {
    if(e instanceof Method) {
        return (Method)e;
    } else {
      return null;
    }
  }

}
