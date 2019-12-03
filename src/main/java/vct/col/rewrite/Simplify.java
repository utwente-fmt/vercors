package vct.col.rewrite;

import vct.col.ast.util.ASTFrame;
import vct.col.ast.generic.ASTNode;
import vct.col.ast.type.ASTReserved;
import vct.col.ast.expr.Dereference;
import vct.col.ast.expr.FieldAccess;
import vct.col.ast.expr.MethodInvokation;

public class Simplify extends AbstractRewriter {
  
  public Simplify(ASTFrame<ASTNode> shared){
    super(shared);
  }
  
  public void visit(Dereference d){
    if (d.obj().isReserved(ASTReserved.This)){
      result = create.local_name(d.field());
    } else {
      super.visit(d);
    }
  }
  
  public void visit(FieldAccess d){
    if (d.object().isReserved(ASTReserved.This)) {
      result = create.local_name(d.name());
    } else {
      super.visit(d);
    }
  }
  
  public void visit(MethodInvokation e){
    ASTNode object;
    if (e.object.isReserved(ASTReserved.This)){
      object=null;
    } else {
      object=rewrite(e.object);
    }
    int N=e.getArity();
    ASTNode args[]=new ASTNode[N];
    for(int i=0;i<N;i++){
      args[i]=e.getArg(i).apply(this);
    }
    MethodInvokation res=create.invokation(object,rewrite(e.dispatch),e.method,args);
    res.set_before(rewrite(e.get_before()));
    res.set_after(rewrite(e.get_after()));
    result=res;
  }
}
