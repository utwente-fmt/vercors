package vct.col.rewrite;

import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;

public class StripConstructors extends AbstractRewriter {

  public StripConstructors(ProgramUnit source) {
    super(source);
  }

  public void visit(Method m){
    if(m.kind==Method.Kind.Constructor){
      result=null;
    } else{
      super.visit(m);
    }
  }
}
