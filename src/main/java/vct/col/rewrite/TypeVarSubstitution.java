package vct.col.rewrite;

import java.util.Map;

import vct.col.ast.type.ClassType;
import vct.col.ast.stmt.decl.ProgramUnit;
import vct.col.ast.type.Type;
import vct.col.ast.type.TypeVariable;

public class TypeVarSubstitution extends AbstractRewriter {
  Map<String,Type> map;
  
  public boolean copy=true;
  
  public TypeVarSubstitution(ProgramUnit source,Map<String,Type> map){
    super(source);
   this.map=map;
  }
  
  public void visit(TypeVariable v){
    Type t = map.get(v.name());
    if (t != null) {
      result = t;
    } else {
      super.visit(v);
    }
  }

  public void visit(ClassType c) {
    if (c.hasArguments()) {
      Type t=map.get(c.getFullName());
      if (t!=null) {
        Warning("type variable %s encoded as class type",c.getFullName());
        result=t;
        return;
      }
    }
    super.visit(c);
  }
}
