package vct.col.ast.util;

import vct.col.ast.generic.ASTNode;
import vct.col.ast.stmt.decl.Method;
import hre.util.Function;

public class MethodFilter implements Function<ASTNode,Method> {

  private Method.Kind kind=null;
  
  public MethodFilter(){}
  
  public MethodFilter(Method.Kind kind){
    this.kind=kind;
  }
  @Override
  public Method apply(ASTNode e) {
    if(e instanceof Method) {
      Method m=(Method)e;
      if (kind==null || m.kind==kind)
        return m ; 
      else 
        return null;
    } else {
      return null;
    }
  }

}
