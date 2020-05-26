package vct.col.rewrite;

import java.util.Arrays;

import vct.col.ast.stmt.decl.Contract;
import vct.col.ast.stmt.decl.DeclarationStatement;
import vct.col.ast.stmt.decl.Method;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.Type;
import vct.col.ast.util.AbstractRewriter;
import vct.col.ast.util.ClassName;

public class FilterClass extends AbstractRewriter {

  private String[] name;
  
  public FilterClass(ProgramUnit source,String name[]){
    super(source);
    this.name=Arrays.copyOf(name,name.length);
  }
  
  public void visit(Method m){
    if (ClassName.prefixOf(name,current_class().getFullName()) || m.kind==Method.Kind.Predicate) {
      // Copy classes that start with the given prefix and all predicates.
      // TODO: copy visible and needed predicates only. 
      super.visit(m);
    } else {
      // Take headers of all other classes.
      // TODO: limit to headers to a minimal set of used classes.
      String name=m.getName();
      DeclarationStatement args[]=rewrite(m.getArgs());
      Contract mc=m.getContract();
      Contract c=null;
      if (mc!=null){
        c=rewrite(mc);
      }
      Method.Kind kind=m.kind;
      Type rt=rewrite(m.getReturnType());
      result=create.method_kind( kind, rt, c, name, args, null);
    }
  }
}
